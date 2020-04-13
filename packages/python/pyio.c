

#include "py4yap.h"
#include <VFS.h>

#include "YapStreams.h"
#include "YapUTF8.h"

YAP_Term TermErrStream, TermOutStream;

static void py_flush(int sno) {
  StreamDesc *st = YAP_GetStreamFromId(sno);
  PyObject *fl = 
    PyObject_GetAttrString(st->u.private_data, "flush");
  if (fl) {
    PyObject_CallFunctionObjArgs(fl,
				 NULL);
  }
#if 0
  //  fprintf(stderr,"%s\n", st->u.w_irl.buf);
  term_t tg = python_acquire_GIL();
  if (st->user_name == TermOutStream){
    PySys_WriteStdout("%s", st->u.w_irl.buf);
  } else {
    PySys_WriteStderr("%s", st->u.w_irl.buf);

  }
  python_release_GIL(tg);
  st->u.w_irl.ptr =   st->u.w_irl.buf;
#endif
}

static int py_putc(int sno, int ch) {
  // PyObject *pyw; // buffer
  // int pyw_kind;
  // PyObject *pyw_data;
  StreamDesc *st = YAP_GetStreamFromId(sno);
#if 0
  if (false && (st->user_name == TermOutStream || st->user_name == TermErrStream)) {
    size_t sz = put_utf8(st->u.w_irl.ptr, ch);
    if (sz > 0) {
      st->u.w_irl.ptr += sz;
      if (  st->u.w_irl.ptr - st->u.w_irl.buf > 256)
	{py_flush(sno); }
    }
    return ch;
  }
#endif
  unsigned char s[2];
  PyObject *err;
  s[0] = ch;
  s[1] = '\0';
  term_t g0 = python_acquire_GIL();
  PyObject_CallMethodObjArgs(st->u.private_data, PyUnicode_FromString("write"),
                             PyUnicode_FromString((char *)s), NULL);
  python_release_GIL(g0);
  if ((err = PyErr_Occurred())) {
    PyErr_SetString(
		    err,
		    "Error in put\n"); // %s:%s:%d!\n", __FILE__, __FUNCTION__, __LINE__);
  }
  return ch;
}

static int py_wputc(int sno, int ch) {
  // PyObject *pyw; // buffer
  // int pyw_kind;
  // PyObject *pyw_data;
  StreamDesc *st = YAP_GetStreamFromId(sno);
#if 0
  if (false && (st->user_name == TermOutStream || st->user_name == TermErrStream)) {
    size_t sz = put_utf8(st->u.w_irl.ptr, ch);
    if (sz > 0) {
      st->u.w_irl.ptr += sz;
      if (  st->u.w_irl.ptr - st->u.w_irl.buf > 256)
	{py_flush(sno); }
    }
    return ch;
  }
#endif
  unsigned char s[8];
  PyObject *err;
  size_t n = put_utf8(s, ch);
  s[n] = '\0';
  term_t g0 = python_acquire_GIL();
  PyObject_CallMethodObjArgs(st->u.private_data, PyUnicode_FromString("write"),
                             PyUnicode_FromString((char *)s), NULL);
  python_release_GIL(g0);
  if ((err = PyErr_Occurred())) {
    PyErr_SetString(
		    err,
		    "Error in put\n"); // %s:%s:%d!\n", __FILE__, __FUNCTION__, __LINE__);
  }
  return ch;
}

VFS_t pystream;
static void *py_open(VFS_t *me, const char *name, const char *io_mode,
                     int sno) {
#if HAVE_STRCASESTR
  if (strcasestr(name, "/python/") == name)
    name += strlen("/python/");
#else
  if (strstr(name, "/python/") == name)
    name += strlen("/python/");
#endif
  term_t ctk = python_acquire_GIL();
  PyObject *pystream = string_to_python(name, true, NULL);
  if (pystream == NULL || pystream == Py_None) {
    python_release_GIL(ctk);
    return NULL;
  } 
  StreamDesc *st = YAP_RepStreamFromId(sno);
  st->name = YAP_LookupAtom(name);
  if (strcmp(name, "sys.stdout") == 0 || strcmp(name, "sys.stderr") == 0 ||
      strcmp(name, "input") == 0) {
    st->status |= Tty_Stream_f;
  }
  /*
    if (!outbuf)
    outbuf =   ( unsigned char *)malloc(1024);
    st->u.w_irl.ptr = st->u.w_irl.buf = outbuf;


    ]\]
    st->user_name = TermOutStream;
    } else if (strcmp(name, "sys.stderr") == 0) {
    st->user_name = TermErrStream;
    if (!errbuf)
    errbuf = ( unsigned char *)malloc(1024);
    st->u.w_irl.ptr = st->u.w_irl.buf = errbuf;
    //  } else if (strcmp(name, "input") == 0) {
    //pystream = PyObject_Call(pystream, PyTuple_New(0), NULL);
    } else */
  { st->user_name = YAP_MkAtomTerm(st->name); }
  st->u.private_data = pystream;
  st->vfs = me;
  st->file = NULL;
  python_release_GIL(ctk);
  if (st->status & (Output_Stream_f | Append_Stream_f))
    py_flush(sno);
  return st;
}

static bool py_close(int sno) {
  StreamDesc *st = YAP_RepStreamFromId(sno);
  if (st->status & (Output_Stream_f | Append_Stream_f))
    py_flush(sno);
  if (strcmp(st->name, "sys.stdout") && strcmp(st->name, "sys.stderr")) {
    Py_XDECREF(st->u.private_data);
    st->u.w_irl.buf = st->u.w_irl.ptr = NULL;
  }
  st->u.private_data = NULL;
  st->vfs = NULL;

  return true;
}

static bool pygetLine(StreamDesc *rl_iostream, int sno) {
  const char *myrl_line;
  PyObject *user_line;
  PyObject *err;
  StreamDesc *s = YAP_GetStreamFromId(sno);
  term_t tg = python_acquire_GIL();
  //  PyObject_Print(s->u.private_data,stderr,0);
  if (PyFunction_Check( s->u.private_data )) {
    user_line = PyObject_CallFunctionObjArgs( s->u.private_data ,
					      NULL);
  } else if ( s->u.private_data == NULL) {
    PyObject *readl = 
      PyObject_GetAttrString(s->u.private_data, "readline");
    if (!readl) {
      readl = 
	PyObject_GetAttrString(s->u.private_data, "input");
    }
    if (readl)
      user_line = PyObject_CallFunctionObjArgs(readl,
					       NULL);
  }
  python_release_GIL(tg);
  if ((err = PyErr_Occurred())) {
    if (PyErr_GivenExceptionMatches(err, PyExc_EOFError))
      return NULL;
    PyErr_Print();
    Yap_ThrowError(SYSTEM_ERROR_GET_FAILED, YAP_MkIntTerm(sno), NULL);
  }
  myrl_line = PyUnicode_AsUTF8(user_line);
  if (myrl_line == NULL)
    return NULL;
  rl_iostream->u.irl.ptr = rl_iostream->u.irl.buf = (unsigned char *)myrl_line;
  return true;
}

static int py_getc(int sno) {
  StreamDesc *s = YAP_RepStreamFromId(sno);
  int ch;
  bool fetch = (s->u.irl.buf == NULL);

  if (fetch) {
    if (!pygetLine(s, sno)) {
      return EOF;
    }
  }
  const unsigned char *ttyptr = s->u.irl.ptr++;
  ch = *ttyptr;
  if (ch == '\0') {
    ch = 10;
  }
  return ch;
}

static int py_wgetc(int sno) {
  StreamDesc *s = YAP_RepStreamFromId(sno);
  int ch;
  bool fetch = (s->u.irl.ptr == NULL);

  if (fetch) {
    if (!pygetLine(s, sno)) {
      return EOF;
    }
  }
  if (s->u.irl.ptr == NULL)
    return 10; // ????
  const unsigned char *ttyptr = s->u.irl.ptr;
  if (*ttyptr == '\0') {
    ch = 10;
    s->u.irl.ptr = NULL;
  } else {
    size_t n = get_utf8(ttyptr, strlen((char *)ttyptr), &ch);
    s->u.irl.ptr += n;
  }
  return ch;
}

/**
   @brief  Yap_ReadlinePeekChar peeks the next char from the
   readline buffer, but does not actually grab it.

   The idea is to take advantage of the buffering. Special care must be taken
   with EOF, though.

*/
static int py_peek(int sno) {
  StreamDesc *s = YAP_RepStreamFromId(sno);
  int ch;

  if (s->u.irl.buf) {
    const unsigned char *ttyptr = s->u.irl.ptr;
    ch = *ttyptr;
    if (ch == '\0') {
      ch = '\n';
    }
    return ch;
  }
  if (pygetLine(s, sno)) {
    ch = s->u.irl.ptr[0];
    if (ch == '\0') {
      ch = '\n';
    }
  } else {
    return EOF;
  }
  return ch;
}

static int64_t py_seek(int sno, int64_t where, int how) {
  StreamDesc *g0 = YAP_RepStreamFromId(sno);
  term_t s0 = python_acquire_GIL();
  PyObject *fseek =  PyObject_GetAttrString(g0->u.private_data, "seek");
  PyObject *pyr = PyObject_CallFunctionObjArgs(fseek, PyLong_FromLong(where),
                                               PyLong_FromLong(how), NULL);
  python_release_GIL(s0);
  return PyLong_AsLong(pyr);
}

#if 0
static void python_output(void) {
  PyObject *stream = string_to_python("sys.stdout", true, NULL);
  StreamDesc *st = YAP_GetStreamFromId(1);
  st->u.private_data = stream;
  st->vfs = &pystream;
  stream = string_to_python("sys.stderr", true, NULL);
  st = YAP_GetStreamFromIds(2);
  st->u.private_data = stream;
  st->vfs = &pystream;
}
#endif

static bool initialized = false;

bool init_python_vfs(void) {
  // pyw = PyUnicode_FromString("x");
  // pyw_kind = PyUnicode_KIND(pyw);
  // pyw_data = PyUnicode_DATA(pyw);
  if (initialized)
    return false;
  initialized = true;
  pystream.name = "python stream";
  pystream.vflags =
    VFS_CAN_WRITE | VFS_CAN_EXEC | VFS_CAN_READ | VFS_HAS_PREFIX;
  pystream.prefix = "/python/";
  pystream.suffix = NULL;
  pystream.open = py_open;
  pystream.close = py_close;
  pystream.get_char = py_getc;
  pystream.get_wchar = py_wgetc;
  pystream.peek_char = py_peek;
  pystream.put_char = py_putc;
  pystream.put_wchar = py_wputc;
  pystream.flush = py_flush;
  pystream.seek = py_seek;
  pystream.next = GLOBAL_VFS;
  GLOBAL_VFS = &pystream;
  TermOutStream = YAP_MkAtomTerm(YAP_LookupAtom("std.output"));
  TermErrStream = YAP_MkAtomTerm(YAP_LookupAtom("std.error"));
  // NULL;
  return true;
}
