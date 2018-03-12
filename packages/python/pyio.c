

#include "py4yap.h"
#include <VFS.h>

#include "YapStreams.h"

YAP_Term TermErrStream, TermOutStream;

static int py_put(int sno, int ch)
{
  // PyObject *pyw; // buffer
  // int pyw_kind;
  // PyObject *pyw_data;
  StreamDesc *st = YAP_GetStreamFromId(sno);
  if (st->user_name == TermOutStream) {
    PyGILState_STATE tg = python_acquire_GIL();
    PySys_WriteStdout("%C", ch);
      python_release_GIL(tg);
   return ch;
  }
  if (st->user_name == TermErrStream) {
     PyGILState_STATE tg = python_acquire_GIL();
   PySys_WriteStderr("%C", ch);
      python_release_GIL(tg);
    return ch;
  }
  char s[2];
  PyObject *err;
  s[0] = ch;
  s[1] = '\0';
   PyGILState_STATE g0 = python_acquire_GIL();
  PyObject_CallMethodObjArgs(st->u.private_data, PyUnicode_FromString("write"),
                             PyUnicode_FromString(s), NULL);
  python_release_GIL(g0);
 if ((err = PyErr_Occurred()))
  {
    PyErr_SetString(
        err,
        "Error in put\n"); // %s:%s:%d!\n", __FILE__, __FUNCTION__, __LINE__);
  }
  return ch;
}

VFS_t pystream;
 static void *py_open(VFS_t *me, int sno, const char *name, const char *io_mode) {
#if HAVE_STRCASESTR
  if (strcasestr(name, "/python/") == name)
    name += strlen("/python/");
#else
  if (strstr(name, "/python/") == name)
    name += strlen("/python/");
#endif
  StreamDesc *st = YAP_RepStreamFromId(sno);
  if  (strcmp(name,"sys.output") == 0) {
    st->user_name = TermOutStream;
  } else if(strcmp(name,"sys.error") == 0) {
    st->user_name = TermErrStream;
  } else {
    // we assume object is already open, so there is no need to open it.
    PyObject *pystream = string_to_python(name, true, NULL);
    if (pystream == Py_None) {
      return NULL;
    } else  {
      st->u.private_data = pystream;
      st->vfs = me;
      st->name = YAP_LookupAtom(name);
      st->user_name = YAP_MkAtomTerm(st->name);
    }
  }
  return st;
}

static bool py_close(int sno) {
    StreamDesc *st = YAP_RepStreamFromId(sno);
  Py_DECREF(st->u.private_data);
  return true;
}

static bool getLine(int inp) {
  char *myrl_line = NULL;
    StreamDesc *rl_instream = YAP_RepStreamFromId(inp);
  PyObject*prompt  = PyUnicode_FromString( "?- "),
  *msg = PyUnicode_FromString("");
  /* window of vulnerability opened */
  myrl_line = PyUnicode_AsUTF8(PyObject_CallFunctionObjArgs(rl_instream->u.private_data,msg,prompt,NULL));
  rl_instream->u.irl.ptr = rl_instream->u.irl.buf = (const unsigned char*)myrl_line;
  myrl_line = NULL;
  return true;
}
 

static int py_getc(int sno) {
  StreamDesc *s = YAP_RepStreamFromId(sno);
  int ch;
  bool fetch = (s->u.irl.buf == NULL);

  if (!fetch || getLine(sno)) {
    const unsigned char *ttyptr = s->u.irl.ptr++, *myrl_line = s->u.irl.buf;
    ch = *ttyptr;
    if (ch == '\0') {
      ch = '\n';
      free((void *)myrl_line);
      s->u.irl.ptr = s->u.irl.buf = NULL;
    }
  } else {
    return EOF;
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
  if (getLine(sno)) {
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
  PyGILState_STATE s0 =   python_acquire_GIL();
  PyObject *fseek = PyObject_GetAttrString(g0->u.private_data, "seek");
  PyObject *pyr = PyObject_CallFunctionObjArgs(fseek, PyLong_FromLong(where),
                                               PyLong_FromLong(how), NULL);
    python_release_GIL(s0);
return PyLong_AsLong(pyr);
}

static void py_flush(int sno) {
  StreamDesc *s = YAP_GetStreamFromId(sno);
  YAP_Term tg =   python_acquire_GIL();
  PyObject *flush = PyObject_GetAttrString(s->u.private_data, "flush");
  PyObject_CallFunction(flush, NULL);
    python_release_GIL(tg);
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
  pystream.peek_char = py_peek;
  pystream.put_char = py_put;
  pystream.flush = py_flush;
  pystream.seek = py_seek;
  pystream.next = GLOBAL_VFS;
  GLOBAL_VFS = &pystream;
  TermOutStream = YAP_MkAtomTerm(YAP_LookupAtom("std.output"));
  TermErrStream = YAP_MkAtomTerm(YAP_LookupAtom("std.error"));
  // NULL;
  return true;
}
