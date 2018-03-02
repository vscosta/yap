

#include "py4yap.h"
#include <VFS.h>

#include "YapStreams.h"

VFS_t pystream;

static void *py_open(VFS_t *me, int sno, const char *name,
                     const char *io_mode) {
#if HAVE_STRCASESTR
  if (strcasestr(name, "/python/") == name)
    name += strlen("/python/");
#else
  if (strstr(name, "/python/") == name)
    name += strlen("/python/");
#endif
  StreamDesc *st = YAP_RepStreamFromId(sno);
  // we assume object is already open, so there is no need to open it.
  PyObject *stream = string_to_python(name, true, NULL);
  if (stream == Py_None)
    return NULL;
  Py_INCREF(stream);
  st->u.private_data = stream;
  st->vfs = me;
  st->name = YAP_LookupAtom(name);
  st->user_name = YAP_MkAtomTerm(st->name);
  return stream;
}

static bool py_close(int sno) { return true; }

static int py_put(int sno, int ch) {
  // PyObject *pyw; // buffer
  // int pyw_kind;
  // PyObject *pyw_data;
  // PySys_WriteStdout("%C", ch);
  //  return ch;
  char s[2];
  StreamDesc *st = YAP_GetStreamFromId(sno);
  //  PyUnicode_WRITE(pyw_kind, pyw_data, 0, ch);
  PyObject *err, *fput = PyObject_GetAttrString(st->u.private_data, "write");
  s[0] = ch;
  s[1] = '\0';
  PyObject_CallMethodObjArgs(st->u.private_data, PyUnicode_FromString("write"),
                             PyUnicode_FromString(s), NULL);
  if ((err = PyErr_Occurred())) {
    PyErr_SetString(
        err,
        "Error in put\n"); // %s:%s:%d!\n", __FILE__, __FUNCTION__, __LINE__);
  }
  return ch;
}

static int py_get(int sno) {
  StreamDesc *s = YAP_GetStreamFromId(sno);
  PyObject *fget = PyObject_GetAttrString(s->u.private_data, "read");
  PyObject *pyr = PyObject_CallFunctionObjArgs(fget, PyLong_FromLong(1), NULL);
  return PyUnicode_READ_CHAR(pyr, 0);
}

static int py_peek(int sno) {
  StreamDesc *s = YAP_GetStreamFromId(sno);
  PyObject *fget = PyObject_GetAttrString(s->u.private_data, "peek");
  PyObject *pyr = PyObject_CallFunctionObjArgs(fget, PyLong_FromLong(1), NULL);
  return PyUnicode_READ_CHAR(pyr, 0);
}

static int64_t py_seek(int sno, int64_t where, int how) {
  StreamDesc *s = YAP_GetStreamFromId(sno);
  PyObject *fseek = PyObject_GetAttrString(s->u.private_data, "seek");
  PyObject *pyr = PyObject_CallFunctionObjArgs(fseek, PyLong_FromLong(where),
                                               PyLong_FromLong(how), NULL);
  return PyLong_AsLong(pyr);
}

static void py_flush(int sno) {
  StreamDesc *s = YAP_GetStreamFromId(sno);
  PyObject *flush = PyObject_GetAttrString(s->u.private_data, "flush");
  PyObject_CallFunction(flush, NULL);
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
  pystream.get_char = py_get;
  pystream.peek_char = py_peek;
  pystream.put_char = py_put;
  pystream.flush = py_flush;
  pystream.seek = py_seek;
  pystream.next = GLOBAL_VFS;
  GLOBAL_VFS = &pystream;
  // NULL;
  return true;
}
