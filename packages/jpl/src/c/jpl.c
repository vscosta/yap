/*  Part of JPL -- SWI-Prolog/Java interface

    Author:        Paul Singleton, Fred Dushin and Jan Wielemaker
    E-mail:        paul@jbgb.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2017, Paul Singleton
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
this source file (jpl.c)  combines   my  Prolog-calls-Java stuff (mostly
prefixed 'JNI' or 'jni'  here)  with   my  adaptation  of  Fred Dushin's
Java-calls-Prolog stuff (mostly prefixed 'JPL' or 'jpl' here)

recent fixes:
 * using PL_get_pointer(), PL_put_pointer() consistently (?)
 * replaced all "Class:  jpl_fli_PL" by "Class:  jpl_fli_Prolog"

still to do:
 * make it completely thread-safe
   (both to multiple Prolog (engine-enabled) threads and to multiple
    Java threads)
 * suss JVM 'abort' and 'exit' handling, and 'vfprintf' redirection
 * rationalise initialisation; perhaps support startup from C?

refactoring (trivial):
 * initialise a functor_t for jpl_tidy_iref_type_cache/1
 * initialise a functor_t for -/2
 * initialise a module_t for jpl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* update this to distinguish releases of this C library: */
#define JPL_C_LIB_VERSION        "7.4.0-alpha"
#define JPL_C_LIB_VERSION_MAJOR  7
#define JPL_C_LIB_VERSION_MINOR  4
#define JPL_C_LIB_VERSION_PATCH  0
#define JPL_C_LIB_VERSION_STATUS "alpha"


#ifdef DEBUG
#undef DEBUG
#endif
#ifndef DEBUG_LEVEL
#define DEBUG_LEVEL 4
#endif
#define DEBUG(n, g) (n >= DEBUG_LEVEL ? g : (void)0)


/* disable type-of-ref caching (at least until GC issues are resolved) */
#define JPL_CACHE_TYPE_OF_REF FALSE

/*=== includes ============================================================ */

/* SWI-Prolog headers: */
#include <config.h>
#ifdef _YAP_NOT_INSTALLED_
#define Sdprintf printf
#else
#include <SWI-Stream.h>
#endif
#include <SWI-Prolog.h>

#ifdef __WINDOWS__
/* OS-specific header (SWI-Prolog FLI and Java Invocation API both seem to need
 * this): but not if we use the .NET 2.0 C compiler
 */
#include <windows.h>
#define SIZEOF_WCHAR_T   2
#define SIZEOF_LONG      4
#define SIZEOF_LONG_LONG 8
#ifdef WIN64
#define SIZEOF_VOIDP 8
#else
#define SIZEOF_VOIDP 4
#endif
#define USE_WIN_EVENTS 1
#endif

/* Java Native Interface and Invocation Interface header: */
#include <jni.h>

/* ANSI/ISO C library header (?): */
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

/* POSIX 'pthreads' headers (initially for JPL's Prolog engine pool, useful for
 * locking generally?):
 */
#include <pthread.h>

#include <assert.h>

/*=== JNI constants ======================================================= */

#define JNI_MIN_JCHAR 0
#define JNI_MAX_JCHAR 65535

#define JNI_MIN_JBYTE -128
#define JNI_MAX_JBYTE 127

#define JNI_MIN_JSHORT -32768
#define JNI_MAX_JSHORT 32767

#define JNI_XPUT_VOID 0
#define JNI_XPUT_BOOLEAN 1
#define JNI_XPUT_BYTE 2
#define JNI_XPUT_CHAR 3
#define JNI_XPUT_SHORT 4
#define JNI_XPUT_INT 5
#define JNI_XPUT_LONG 6
#define JNI_XPUT_FLOAT 7
#define JNI_XPUT_DOUBLE 8
#define JNI_XPUT_FLOAT_TO_DOUBLE 9
#define JNI_XPUT_LONG_TO_FLOAT 10
#define JNI_XPUT_LONG_TO_DOUBLE 11
#define JNI_XPUT_REF 12
#define JNI_XPUT_ATOM 13
#define JNI_XPUT_JVALUEP 14
#define JNI_XPUT_JVALUE 15

/* JNI "hashed refs" constants */

#define JNI_HR_LOAD_FACTOR 0.75

/* jni_hr_add() return codes: */
#define JNI_HR_ADD_FAIL -1
#define JNI_HR_ADD_NEW 0
#define JNI_HR_ADD_OLD 1

/*=== JPL constants ======================================================= */

/* legit values for jpl_status_jpl_ini and jpl_status_pvm_ini */
#define JPL_INIT_RAW 101
#define JPL_INIT_PVM_MAYBE 102
#define JPL_INIT_OK 103
#define JPL_INIT_JPL_FAILED 104
#define JPL_INIT_PVM_FAILED 105

#define JPL_MAX_POOL_ENGINES 1    /* max pooled Prolog engines */
#define JPL_INITIAL_POOL_ENGINES 1 /* initially created ones */

/* legit values for jpl_syntax */
#define JPL_SYNTAX_UNDEFINED 201
#define JPL_SYNTAX_TRADITIONAL 202
#define JPL_SYNTAX_MODERN 203

/*=== JNI initialisation macro (typically succeeds cheaply) =============== */

#define jni_ensure_jvm() \
  ((jvm != NULL || jni_create_default_jvm()) && (env = jni_env()) != NULL)

/*=== JPL initialisation macros (typically succeed cheaply) =============== */

/* outcomes: */
/*      fail to find org.jpl7.*, org.jpl7.fli.* classes or to convert init args
 * to String[]: exception, FALSE */
/*      JPL is (newly or already) out of RAW state: TRUE */
#define jpl_ensure_jpl_init(e) \
  (jpl_status != JPL_INIT_RAW || jpl_ensure_jpl_init_1(e))
/* outcomes: */
/*              JPL or PVM init has already failed: FALSE */
/*              JPL or PVM init fails while being necessarily attempted: exception
 */
/*              JPL is (newly or already) fully initialised: TRUE */
#define jpl_ensure_pvm_init(e) \
  (jpl_status == JPL_INIT_OK || jpl_ensure_pvm_init_1(e))

/*=== types (structs and typedefs) ======================================== */

typedef struct Hr_Entry HrEntry; /* enables circular definition... */

struct Hr_Entry                         /* a single interned reDEference */
{ jobject  obj;                         /* a JNI global ref */
  int      hash;                        /* identityHashCode(obj) */
  HrEntry *next;                        /* next entry in this chain, or NULL */
};

typedef struct Hr_Table HrTable;

struct Hr_Table
{ int       count;                      /* current # entries */
  int       threshold;                  /* rehash on add when count==threshold */
  int       length;                     /* # slots in slot array */
  HrEntry **slots;                      /* pointer to slot array */
};


typedef intptr_t *pointer;               /* for JPL */
#ifndef _YAP_NOT_INSTALLED_
typedef int bool;                       /* for JNI/JPL functions returning */
#endif                                        /* only TRUE or FALSE */

/*=== JNI constants: sizes of JNI primitive types ========================= */

int size[16] = { /* NB relies on sequence of JNI_XPUT_* defs */
    0,
    sizeof(jboolean), /* size[JNI_XPUT_BOOLEAN] */
    sizeof(jbyte),    /* size[JNI_XPUT_BYTE] */
    sizeof(jchar),    /* size[JNI_XPUT_CHAR] */
    sizeof(jshort),   /* size[JNI_XPUT_SHORT] */
    sizeof(jint),     /* size[JNI_XPUT_INT] */
    sizeof(jlong),    /* size[JNI_XPUT_LONG] */
    sizeof(jfloat),   /* size[JNI_XPUT_FLOAT] */
    sizeof(jdouble),  /* size[JNI_XPUT_DOUBLE] */
    0,                /* n/a - JNI_FLOAT_TO_DOUBLE */
    0,                /* n/a - JNI_LONG_TO_FLOAT */
    0,                /* n/a - JNI_LONG_TO_DOUBLE */
    0,                /* n/a - JNI_REF */
    0,                /* n/a - JNI_ATOM */
    0,                /* n/a - JNI_JVALUEP */
    sizeof(jvalue)    /* size[JNI_XPUT_JVALUE] */
};

/*=== JNI "constants", lazily initialised by jni_init() =================== */

static atom_t JNI_atom_false;           /* false */
static atom_t JNI_atom_true;            /* true */

static atom_t JNI_atom_boolean;         /* boolean */
static atom_t JNI_atom_char;            /* char */
static atom_t JNI_atom_byte;            /* byte */
static atom_t JNI_atom_short;           /* short */
static atom_t JNI_atom_int;             /* int */
static atom_t JNI_atom_long;            /* long */
static atom_t JNI_atom_float;           /* float */
static atom_t JNI_atom_double;          /* double */

static atom_t JNI_atom_null;            /* null */
static atom_t JNI_atom_void;            /* void */

#if _YAP_NOT_INSTALLED_
static  atom_t ATOM_nil;
#endif

static functor_t JNI_functor_at_1;             /* @(_) */
static functor_t JNI_functor_jbuf_2;           /* jbuf(_,_) */
static functor_t JNI_functor_jlong_2;          /* jlong(_,_) */
static functor_t JNI_functor_jfieldID_1;       /* jfieldID(_) */
static functor_t JNI_functor_jmethodID_1;      /* jmethodID(_) */
static functor_t JNI_functor_error_2;          /* error(_, _) */
static functor_t JNI_functor_java_exception_1; /* java_exception(_) */
static functor_t JNI_functor_jpl_error_1;      /* jpl_error(_) */
static functor_t JNI_functor_pair_2;           /* _-_ */

/*=== JNI's static JVM references, lazily initialised by jni_init() ======= */

static jclass c_class;          /* java.lang.Class (jClass_c?) */
static jmethodID c_getName;     /* java.lang.Class'getName() (jClassGetName_m?) */
static jclass str_class;        /* java.lang.String (duplicates jString_c below)*/
static jclass term_class;       /* org.jpl7.Term */
static jclass termt_class;      /* org.jpl7.fli.term_t */

static jclass sys_class;        /* java.lang.System (rename to jSystem_c?) */
static jmethodID sys_ihc;       /* java.lang.System's identityHashCode()
                                   (rename to jSystemIdentityHashCode_m?) */
static jmethodID term_getTerm;  /* org.jpl7.Term's getTerm() */
static jmethodID term_put;      /* org.jpl7.Term's put() */
static jmethodID term_putTerm;  /* org.jpl7.Term's static putTerm(Term,term_t) */

/* === JPL's reusable global class object refs, initialised by
 * jpl_ensure_jpl_init() ================
 */

static jclass jString_c;
static jclass jJPLException_c;
static jclass jTermT_c;
static jclass jAtomT_c;
static jclass jFunctorT_c;
static jclass jFidT_c;
static jclass jPredicateT_c;
static jclass jQidT_c;
static jclass jModuleT_c;
static jclass jEngineT_c;

static jclass jLongHolder_c;
static jclass jPointerHolder_c;
static jclass jIntHolder_c;
static jclass jInt64Holder_c;
static jclass jDoubleHolder_c;
static jclass jStringHolder_c;
static jclass jObjectHolder_c;
static jclass jBooleanHolder_c;

/*=== JPL's reusable constant field IDs, set before first use by
 * jpl_ensure_jpl_init() ============= */

static jfieldID jLongHolderValue_f;
static jfieldID jPointerHolderValue_f;
static jfieldID jIntHolderValue_f;
static jfieldID jInt64HolderValue_f;
static jfieldID jDoubleHolderValue_f;
static jfieldID jStringHolderValue_f;
static jfieldID jObjectHolderValue_f;
static jfieldID jBooleanHolderValue_f;

/*=== JPL's default args for PL_initialise() (NB these are not really good
 * enough) ================= */

const char *default_args[] =
#if __YAP_PROLOG__
        { "yap", "-nosignals", NULL };
#else
{ "swipl", "-g", "true", "--nosignals", NULL };
#endif

/*=== JNI global state (initialised by jni_create_jvm_c) ================== */

static JavaVM *jvm = NULL;              /* non-null -> JVM successfully
                                           loaded & initialised */
static char * jvm_ia[2] = {"-Xrs", NULL};
static char **jvm_dia = jvm_ia;         /* default JVM init args (after
                                           jpl init, until jvm init) */
static char **jvm_aia = NULL;           /* actual JVM init args (after jvm init)*/

/*=== JNI global state (hashed global refs) =============================== */

static HrTable *hr_table = NULL;        /* handle to allocated-on-demand table */
static int64_t hr_add_count = 0;        /* total of new refs interned */
static int64_t hr_old_count = 0;        /* total of old refs reused */
static int64_t hr_del_count = 0;        /* total of dead refs released */

/*=== JPL global state, initialised by jpl_ensure_jpl_init() or
 * jpl_ensure_jvm_init() ============== */

static int jpl_status = JPL_INIT_RAW;   /* neither JPL nor PVM initialisation
                                           has occurred */
static jobject pvm_dia = NULL;          /* default PVM init args (after
                                           jpl init, until pvm init) */
static jobject      pvm_aia = NULL;     /* actual PVM init args (after pvm init)*/
static PL_engine_t *engines = NULL;     /* handles of the pooled Prolog engines */
static int          engines_allocated = 0; /* size of engines array */
#ifdef USE_WIN_EVENTS
static HANDLE engines_event;
static CRITICAL_SECTION engines_mutex;
#else
static pthread_cond_t engines_cond   = PTHREAD_COND_INITIALIZER;  /* pool access*/
static pthread_mutex_t engines_mutex = PTHREAD_MUTEX_INITIALIZER; /* pool access*/
#endif

static pthread_mutex_t jvm_init_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t pvm_init_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t jref_mutex     = PTHREAD_MUTEX_INITIALIZER;

static int jpl_syntax = JPL_SYNTAX_UNDEFINED; /* init sets
                                                 JPL_SYNTAX_TRADITIONAL or
                                                 JPL_SYNTAX_MODERN */

/*=== common functions ==================================================== */

JNIEnv *
jni_env(void) /* economically gets a JNIEnv pointer, valid for this thread */
{ JNIEnv *env;

  switch ((*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION_1_2))
  { case JNI_OK:
      return env;
    case JNI_EDETACHED:
      DEBUG(2, Sdprintf("[JPL: jni_env() calls AttachCurrentThread]\n"));
      return (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL) == 0 ? env
                                                                        : NULL;
    default: /* error */
      return NULL;
  }
}

static char *
jpl_c_lib_version(void)
{ static char  v[100];    /* version string */
  static char *vp = NULL; /* set to v at first call */

  if (vp != NULL) /* already set? */
  { return vp;
  }
  sprintf(v, "%d.%d.%d-%s", JPL_C_LIB_VERSION_MAJOR, JPL_C_LIB_VERSION_MINOR,
          JPL_C_LIB_VERSION_PATCH, JPL_C_LIB_VERSION_STATUS);
  vp = v;
  return vp;
}

/* ta:  -atom: this library's version as an atom, e.g. '3.1.0-alpha'
*/
static foreign_t
jpl_c_lib_version_1_plc(term_t ta)
{ return PL_unify_atom_chars(ta, jpl_c_lib_version());
}

static foreign_t
jpl_c_lib_version_4_plc(term_t tmajor,  /* -integer: major version number */
                        term_t tminor,  /* -integer: minor version number */
                        term_t tpatch,  /* -integer: patch version number */
                        term_t tstatus) /* -atom: status of this version */
{ return PL_unify_integer(tmajor, JPL_C_LIB_VERSION_MAJOR) &&
         PL_unify_integer(tminor, JPL_C_LIB_VERSION_MINOR) &&
         PL_unify_integer(tpatch, JPL_C_LIB_VERSION_PATCH) &&
         PL_unify_atom_chars(tstatus, JPL_C_LIB_VERSION_STATUS);
}

/*=== JNI function prototypes (to resolve unavoidable forward references) = */

static int  jni_hr_add(JNIEnv *, jobject, pointer *);
static int  jni_hr_del(JNIEnv *, pointer);
static bool jni_free_iref(JNIEnv *env, pointer iref);
static bool jni_object_to_iref(JNIEnv *env, jobject obj, pointer *iref);
static bool jni_String_to_atom(JNIEnv *env, jobject s, atom_t *a);
static bool jni_atom_to_String(JNIEnv *env, atom_t a, jobject *s);


                /*******************************
                *          JREF SYMBOL         *
                *******************************/

typedef struct jref_handle
{ pointer iref;
} jref_handle;

static int
write_jref_handle(IOSTREAM *s, atom_t jref, int flags)
{ jref_handle *ref = PL_blob_data(jref, NULL, NULL);
  (void)flags;

#if __YAP_PROLOG__
  fprintf(stderr, "<jref>(%p)", ref->iref);
#else
  Sfprintf(s, "<jref>(%p)", ref->iref);
#endif
  return TRUE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
release_jref_handle() is called from AGC. As  the symbol is destroyed,
we must clear info->symbol. That is find   as AGC locks L_THREAD and the
competing interaction in free_jref_info() is also locked with L_THREAD
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
release_jref_handle(atom_t jref)
{ jref_handle *ref = PL_blob_data(jref, NULL, NULL);
  JNIEnv *     env;
  
  if ((env = jni_env()))
  { if (!jni_free_iref(env, ref->iref))
      DEBUG(0, Sdprintf("[JPL: garbage-collected jref<%p> is bogus (not in "
                        "HashedRefs)]\n",
                        ref->iref));
  }

  return TRUE;
}

static int
save_jref(atom_t jref, IOSTREAM *fd)
{ jref_handle *ref = PL_blob_data(jref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <jref>(%p)", ref->iref);
}

static atom_t
load_jref(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-jref-handle>");
}

static PL_blob_t jref_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  "jref",
  release_jref_handle,
  NULL,
  write_jref_handle,
  NULL,
  save_jref,
  load_jref
};

static bool
jni_tag_to_iref(atom_t a, pointer *iref)
{ PL_blob_t *  type;
  jref_handle *ref;

  if ((ref = PL_blob_data(a, NULL, &type)) && type == &jref_blob)
  { *iref = ref->iref;
    return TRUE;
  }

  return FALSE;
}

/* === JNI Prolog<->Java conversion functions ============================== */

/* JNI (Prolog-calls-Java) conversion functions; mainly used in
 * jni_{func|void}_{0|1|2|3|4}_plc; */

static bool
jni_term_to_jboolean(term_t t, jboolean *jz)
{ functor_t fn;
  term_t    a1;
  atom_t    a;

  if (PL_get_functor(t, &fn) && fn == JNI_functor_at_1)
  { a1 = PL_new_term_ref();
    if (PL_get_arg(1, t, a1) && PL_get_atom(a1, &a))
    { if (a == JNI_atom_false)
      { *jz = 0;
        return TRUE;
      }
      if (a == JNI_atom_true)
      { *jz = 1;
        return TRUE;
      }
    }
  }
  return FALSE;
}

static bool
jni_term_to_jchar(term_t t, jchar *jc)
{ int i;

  if (PL_get_integer(t, &i) && i >= JNI_MIN_JCHAR && i <= JNI_MAX_JCHAR)
  { *jc = (jchar)i;
    return TRUE;
  }

  return FALSE;
}

static bool
jni_term_to_jbyte(term_t t, jbyte *jb)
{ int i;

  if (PL_get_integer(t, &i) && i >= JNI_MIN_JBYTE && i <= JNI_MAX_JBYTE)
  { *jb = (jbyte)i;
    return TRUE;
  }

  return FALSE;
}

static bool
jni_term_to_jshort(term_t t, jshort *js)
{ int i;

  if (PL_get_integer(t, &i) && i >= JNI_MIN_JSHORT && i <= JNI_MAX_JSHORT)
  { *js = (jshort)i;
    return TRUE;
  }

  return FALSE;
}

/* JW: jint is always 32-bit! */

static bool
jni_term_to_jint(term_t t, jint *ji)
{ int i;

  if (PL_get_integer(t, &i))
  { *ji = i;
    return TRUE;
  }

  return FALSE;
}

static bool
jni_term_to_jsize(term_t t, jsize *js)
{ int i;

  if (PL_get_integer(t, &i))
  { *js = i;
    return TRUE;
  }

  return FALSE;
}

static bool
jni_term_to_non_neg_jsize(term_t t, jsize *js)
{ intptr_t i;

  if (PL_get_intptr(t, &i) && i >= 0)
  { *js = (jsize)i;
    return TRUE;
  }

  return FALSE;
}

static bool
jni_term_to_jlong(term_t t, jlong *jl)
{ int64_t i64;

  if (PL_get_int64(t, &i64))
  { *jl = (jlong)i64;
    return TRUE;
  }

  return FALSE;
}

static bool
jni_term_to_jfloat(term_t t, jfloat *jf)
{ double  d;
  int64_t i64;

  if (PL_get_float(t, &d))
  { *jf = (jfloat)d;
    return TRUE;
  }

  if (PL_get_int64(t, &i64))
  { *jf = (jfloat)i64;
    return TRUE;
  }

  return FALSE;
}

static bool
jni_term_to_jdouble(term_t t, jdouble *jd)
{ double  d;
  int64_t i64;

  if (PL_get_float(t, &d))
  { *jd = (jdouble)d;
    return TRUE;
  }

  if (PL_get_int64(t, &i64))
  { *jd = (jdouble)i64;
    return TRUE;
  }

  return FALSE;
}

static bool
jni_term_to_jfieldID(term_t t, jfieldID *jf)
{ functor_t fn;
  term_t    a1;

  if (PL_get_functor(t, &fn) && fn == JNI_functor_jfieldID_1)
  { a1 = PL_new_term_ref();
    if (PL_get_arg(1, t, a1) && PL_get_pointer(a1, (void **)jf))
      return TRUE;
  }

  return FALSE;
}

static bool
jni_term_to_jmethodID(term_t t, jmethodID *jm)
{ functor_t fn;
  term_t    a1;

  if (PL_get_functor(t, &fn) && fn == JNI_functor_jmethodID_1)
  { a1 = PL_new_term_ref();
    if (PL_get_arg(1, t, a1) && PL_get_pointer(a1, (void **)jm))
      return TRUE;
  }

  return FALSE;
}

static bool
jni_term_to_ref(term_t t, jobject *j, JNIEnv *env)
{ atom_t       a;
  jref_handle *ref;
  PL_blob_t *  type;
  term_t       a1;

  if (PL_get_atom(t, &a))
  { if ((ref = PL_blob_data(a, NULL, &type)) && type == &jref_blob)
    { *j = (jobject)(ref->iref);
      return TRUE; // <jref>(0x1234560) -> referenced object
    }

    if (jni_atom_to_String(env, a, j))
      return TRUE;

    return FALSE;
  }

  if ( PL_is_functor(t, JNI_functor_at_1) &&
       (a1 = PL_new_term_ref()) &&
       PL_get_arg(1, t, a1) &&
       PL_get_atom(a1, &a) &&
       a == JNI_atom_null)
  { *j = 0;
    return TRUE; // @(null) -> 0
  }

  return FALSE;
}

static bool
jni_term_to_jobject(term_t t, jobject *j, JNIEnv *env)
{ jobject j2;

  return jni_term_to_ref(t, &j2, env) && j2 != NULL && (*j = j2, TRUE);
}

static bool
jni_term_to_jclass(term_t t, jclass *j, JNIEnv *env)
{ jobject j2;

  return jni_term_to_ref(t, &j2, env) && j2 != NULL && (*j = (jclass)j2, TRUE);
}

static bool
jni_term_to_non_array_jclass(term_t t, jclass *j, JNIEnv *env)
{ jobject j2;

  return jni_term_to_ref(t, &j2, env) && j2 != NULL && (*j = (jclass)j2, TRUE);
}

static bool
jni_term_to_jarray(term_t t, jarray *j, JNIEnv *env)
{ jobject j2;
  return jni_term_to_ref(t, &j2, env) && (*j = (jarray)j2, TRUE);
}

static bool
jni_term_to_object_jarray(term_t t, jobjectArray *j, JNIEnv *env)
{ jobject j2;
  return jni_term_to_ref(t, &j2, env) && (*j = (jobjectArray)j2, TRUE);
}

static bool
jni_term_to_boolean_jarray(term_t t, jbooleanArray *j, JNIEnv *env)
{ jobject j2;
  return jni_term_to_ref(t, &j2, env) && (*j = (jbooleanArray)j2, TRUE);
}

static bool
jni_term_to_byte_jarray(term_t t, jbyteArray *j, JNIEnv *env)
{ jobject j2;
  return jni_term_to_ref(t, &j2, env) && (*j = (jbyteArray)j2, TRUE);
}

static bool
jni_term_to_char_jarray(term_t t, jcharArray *j, JNIEnv *env)
{ jobject j2;
  return jni_term_to_ref(t, &j2, env) && (*j = (jcharArray)j2, TRUE);
}

static bool
jni_term_to_short_jarray(term_t t, jshortArray *j, JNIEnv *env)
{ jobject j2;
  return jni_term_to_ref(t, &j2, env) && (*j = (jshortArray)j2, TRUE);
}

static bool
jni_term_to_int_jarray(term_t t, jintArray *j, JNIEnv *env)
{ jobject j2;
  return jni_term_to_ref(t, &j2, env) && (*j = (jintArray)j2, TRUE);
}

static bool
jni_term_to_long_jarray(term_t t, jlongArray *j, JNIEnv *env)
{ jobject j2;
  return jni_term_to_ref(t, &j2, env) && (*j = (jlongArray)j2, TRUE);
}

static bool
jni_term_to_float_jarray(term_t t, jfloatArray *j, JNIEnv *env)
{ jobject j2;
  return jni_term_to_ref(t, &j2, env) && (*j = (jfloatArray)j2, TRUE);
}

static bool
jni_term_to_double_jarray(term_t t, jdoubleArray *j, JNIEnv *env)
{ jobject j2;
  return jni_term_to_ref(t, &j2, env) && (*j = (jdoubleArray)j2, TRUE);
}

/* If `t` is term jbuf(Ptr, `type`), extract Ptr to `vp`
 */

static int
jni_term_to_jbuf(term_t t, atom_t type, void **vp)
{ if ( PL_is_functor(t, JNI_functor_jbuf_2) )
  { term_t a;

    if ( (a=PL_new_term_ref()) )
    { atom_t tp;

      _PL_get_arg(2, t, a);
      if ( PL_get_atom(a, &tp) && tp == type )
      { _PL_get_arg(1, t, a);
        return PL_get_pointer(a, vp);
      }
    }
  }

  return FALSE;
}


static bool
jni_term_to_jboolean_buf(term_t t, jboolean **jzp)
{ return jni_term_to_jbuf(t, JNI_atom_boolean, (void **)jzp);
}

static bool
jni_term_to_jbyte_buf(term_t t, jbyte **jbp)
{ return jni_term_to_jbuf(t, JNI_atom_byte, (void **)jbp);
}

static bool
jni_term_to_jchar_buf(term_t t, jchar **jcp)
{ return jni_term_to_jbuf(t, JNI_atom_char, (void **)jcp);
}

static bool
jni_term_to_jshort_buf(term_t t, jshort **jsp)
{ return jni_term_to_jbuf(t, JNI_atom_short, (void **)jsp);
}

static bool
jni_term_to_jint_buf(term_t t, jint **jip)
{ return jni_term_to_jbuf(t, JNI_atom_int, (void **)jip);
}

static bool
jni_term_to_jlong_buf(term_t t, jlong **jlp)
{ return jni_term_to_jbuf(t, JNI_atom_long, (void **)jlp);
}

static bool
jni_term_to_jfloat_buf(term_t t, jfloat **jfp)
{ return jni_term_to_jbuf(t, JNI_atom_float, (void **)jfp);
}

static bool
jni_term_to_jdouble_buf(term_t t, jdouble **jdp)
{ return jni_term_to_jbuf(t, JNI_atom_double, (void **)jdp);
}

static bool
jni_term_to_charP(term_t t, char **ch)
{ return PL_get_atom_chars(t, ch);
}

static bool
jni_term_to_pointer(term_t t, jvalue **jvp)
{ return PL_get_pointer(t, (void **)jvp);
}

/* JNI Java-to-Prolog conversion functions: */

static bool
jni_jboolean_to_term(jboolean jb, term_t t)
{ return PL_unify_term(t, PL_FUNCTOR, JNI_functor_at_1,
                            PL_ATOM, jb ? JNI_atom_true : JNI_atom_false);
}

static bool
jni_jchar_to_term(jchar jch, term_t t)
{ return PL_unify_integer(t, (int)jch);
}

static bool
jni_jbyte_to_term(jbyte jb, term_t t)
{ return PL_unify_integer(t, (int)jb);
}

static bool
jni_jshort_to_term(jshort js, term_t t)
{ return PL_unify_integer(t, (int)js);
}

static bool
jni_jint_to_term(jint ji, term_t t)
{ return PL_unify_integer(t, (int)ji);
}

static bool
jni_jlong_to_term(jlong jl, term_t t)
{ return PL_unify_int64(t, (int64_t)jl);
}

static bool
jni_jfloat_to_term(jfloat jf, term_t t)
{ return PL_unify_float(t, (double)jf);
}

static bool
jni_jdouble_to_term(jdouble jd, term_t t)
{ return PL_unify_float(t, (double)jd);
}

static bool
jni_jfieldID_to_term(jfieldID jf, term_t t)
{ return PL_unify_term(t, PL_FUNCTOR, JNI_functor_jfieldID_1, PL_POINTER,
                       (void *)jf);
}

static bool
jni_jmethodID_to_term(jmethodID jm, term_t t)
{ return PL_unify_term(t, PL_FUNCTOR, JNI_functor_jmethodID_1, PL_POINTER,
                       (void *)jm);
}

static bool
jni_unify_iref(term_t t, pointer iref)
{ jref_handle jref;

  jref.iref = iref;
  return PL_unify_blob(t, &jref, sizeof(jref), &jref_blob);
}

static bool
jni_jobject_to_term(jobject j, term_t t, JNIEnv *env)
{ pointer i;

  if (j == NULL)
    return PL_unify_term(t, PL_FUNCTOR, JNI_functor_at_1, PL_ATOM,
                         JNI_atom_null);

  if ((*env)->IsInstanceOf(env, j, str_class))
  { atom_t a;

    if (jni_String_to_atom(env, j, &a))
    { int rc = PL_unify_atom(t, a);
      PL_unregister_atom(a);
      return rc;
    }
    return FALSE;
  }

  if (jni_object_to_iref(env, j, &i))
    return jni_unify_iref(t, i);

  assert(0);
  return FALSE;
}

/*
 * obj:  a newly returned JNI local ref
 * iref: gets an integerised, canonical, global equivalent
 */

static bool
jni_object_to_iref(JNIEnv *env, jobject obj, pointer *iref)
{ int r;                                /* temp for result code */

  if ((r = jni_hr_add(env, obj, iref)) == JNI_HR_ADD_NEW)
  { hr_add_count++;                     /* obj was novel, added to dict */
    return TRUE;
  } else if (r == JNI_HR_ADD_OLD)
  { hr_old_count++;                     /* obj was already in dict */
    return TRUE;
  } else
  { return FALSE;                       /* r == JNI_HR_ADD_FAIL, presumably */
  }
}

/* retract all jpl_iref_type_cache(Iref,_) facts */
static bool
jni_tidy_iref_type_cache(pointer iref)
{ term_t goal;

  if (JPL_CACHE_TYPE_OF_REF)
  { return ((goal = PL_new_term_ref()) &&
            PL_unify_term(goal, PL_FUNCTOR_CHARS, "jpl_tidy_iref_type_cache", 1,
                                  PL_INT, iref) &&
            PL_call(goal, PL_new_module(PL_new_atom("jpl"))));
  } else
  { return TRUE;
  }
}

/* Called indirectly from agc hook when a possible iref is unreachable.
 * could merge this into jni_hr_del() ?
 */
static bool
jni_free_iref(JNIEnv *env, pointer iref)
{ if (jni_hr_del(env, iref)) /* iref matched a hashedref table entry?
                                (in which case, was deleted) */
  { if (!jni_tidy_iref_type_cache(iref))
      DEBUG(0, Sdprintf("[JPL: jni_tidy_iref_type_cache(%p) failed]\n", iref));
    hr_del_count++;
    return TRUE;
  } else
  { return FALSE;
  }
}

/* called from jni_jobject_to_term() and org.jpl7.fli.Prolog#new_atom()
 * NB this delivers an atom_t, not a term_t
 * returns FALSE if the String arg is NULL
 */

#define FASTJCHAR 512

static bool
jni_String_to_atom(JNIEnv *env, jobject s, atom_t *a)
{ jsize        len = (*env)->GetStringLength(env, s);
  const jchar *jcp = (*env)->GetStringChars(env, s, NULL);

  if ( s == NULL )
    return FALSE;

#if SIZEOF_WCHAR_T == 2
  { *a = PL_new_atom_wchars(len, jcp); /* easy, huh? (thanks, Jan) */
  }
#else
  { pl_wchar_t tmp[FASTJCHAR];
    pl_wchar_t *wp;
    jsize       i;

    wp = len <= FASTJCHAR ? tmp : malloc(sizeof(pl_wchar_t) * (len+1));
    if ( !wp )
    { (*env)->ReleaseStringChars(env, s, jcp);
      return FALSE;
    }
    for (i = 0; i < len; i++)
      wp[i] = jcp[i];
    wp[i] = '\0';
    *a = PL_new_atom_wchars(len, wp);
    if ( wp != tmp )
      free(wp);
  }
#endif

  (*env)->ReleaseStringChars(env, s, jcp);
  return *a != 0;
}

static bool
jni_new_string(JNIEnv *env, const char *s, size_t len, jobject *obj)
{ jchar tmp[FASTJCHAR];
  jchar *js;
  size_t i;

  js = len <= FASTJCHAR ? tmp : malloc(sizeof(jchar) * len);
  if ( !js )
    return FALSE;

  for (i = 0; i < len; i++)
    js[i] = s[i] & 0xff;

  *obj = (*env)->NewString(env, js, len);
  if ( js != tmp )
    free(js);

  return (*obj != NULL);
}


static bool
jni_new_wstring(JNIEnv *env, const pl_wchar_t *s, size_t len, jobject *obj)
{
#if SIZEOF_WCHAR_T == 2
  return (*obj = (*env)->NewString(env, s, len)) != NULL;
#else
  jchar tmp[FASTJCHAR];
  jchar *js;
  size_t i;

  js = len <= FASTJCHAR ? tmp : malloc(sizeof(jchar) * len);
  if ( !js )
    return FALSE;

  for (i = 0; i < len; i++)
    js[i] = s[i];

  *obj = (*env)->NewString(env, js, len);
  if ( js != tmp )
    free(js);

  return (*obj != NULL);
#endif
}



static bool
jni_atom_to_String(JNIEnv *env, atom_t a, jobject *s)
{ size_t            len;
  const pl_wchar_t *wp;
  const char       *cp;

  if ( (cp = PL_atom_nchars(a, &len)) )
  { return jni_new_string(env, cp, len, s);
  } else if ( (wp = (pl_wchar_t *)PL_atom_wchars(a, &len)) )
  { return jni_new_wstring(env, wp, len, s);
  } else
  { return FALSE;
  }
}

/* checks that the term_t is a string and delivers a String representation of it
 * t: a term which may or may not be a SWIPL string
 */
static bool
jni_string_to_String(JNIEnv *env, term_t t, jobject *s)
{ size_t       len;
  pl_wchar_t * wp;
  char *       cp;

  if ( PL_get_nchars(t, &len, &cp, CVT_ATOM|CVT_STRING) )
  { return jni_new_string(env, cp, len, s);
  } else if ( PL_get_wchars(t, &len, &wp, CVT_ATOM|CVT_STRING) )
  { return jni_new_wstring(env, wp, len, s);
  } else
  { return FALSE;
  }
}

/* an FLI wrapper for jni_tag_to_iref() above is currently called by
 * jpl_tag_to_type/2, jpl_cache_type_of_object/2 jpl_tag_to_type/2
 * is called by jpl_object_to_type/2, jpl_ref_to_type/2
 * tt: +atom: a tag
 * ti: -integer: its corresponding iref
 */
static foreign_t
jni_tag_to_iref_plc(term_t tt, term_t ti)
{ atom_t  a;
  pointer iref;

  return PL_get_atom(tt, &a) &&
         jni_tag_to_iref(a, &iref) &&
         PL_unify_integer(ti, iref);
}

/*=== "hashed ref" (canonical JNI global reference) support =============== */

/* implements jni_hr_info/4
 * t1: -integer: # object references currently in hash table
 * t2: -integer: total # object references so far added
 * t3: -integer: total # object references so far found to be already in table
 * t4: -integer: total # object references deleted from table (by atom GC)
 */
static foreign_t
jni_hr_info_plc(term_t t1, term_t t2, term_t t3, term_t t4 )
{ return PL_unify_integer(t1, (hr_table == NULL ? 0 : hr_table->count)) &&
         PL_unify_int64(t2, hr_add_count) &&
         PL_unify_int64(t3, hr_old_count) &&
         PL_unify_int64(t4, hr_del_count);
}

/* unifies t2 with a Prolog term which represents the contents of the hashtable
 * slot */
static bool
jni_hr_table_slot(term_t t2, HrEntry *slot)
{ term_t tp = PL_new_term_ref();

  if (slot == NULL)
  { return PL_unify_nil(t2);
  } else
  { return PL_unify_list(t2, tp, t2) &&
           PL_unify_term(tp, PL_FUNCTOR, JNI_functor_pair_2,
                               PL_INT, slot->hash,
                               PL_LONG, slot->obj) &&
           jni_hr_table_slot(t2, slot->next);
  }
}

/* unifies t with a list of hash table slot representations */
static foreign_t
jni_hr_table_plc(term_t t)
{ term_t t1 = PL_copy_term_ref(t);
  term_t t2 = PL_new_term_ref();
  int    i;

  for (i = 0; i < hr_table->length; i++)
  { if (!PL_unify_list(t1, t2, t1) ||
        !jni_hr_table_slot(t2, hr_table->slots[i]))
    { return FALSE;
    }
  }
  return PL_unify_nil(t1);
}

/* an empty table of length is successfully created, where none was before
 * length: # slots in table
*/
static bool
jni_hr_create(int length)
{ int i;

  if (hr_table != NULL)
  { return FALSE;                       /* table already exists */
  }
  if (length <= 0)
  { return FALSE;                       /* unsuitable length */
  }
  if ((hr_table = (HrTable *)malloc(sizeof(HrTable))) == NULL)
  { return FALSE;
  }
  hr_table->length    = length;
  hr_table->threshold = (int)(hr_table->length * JNI_HR_LOAD_FACTOR);
  if ((hr_table->slots = (HrEntry **)malloc(length * sizeof(HrEntry *))) == NULL)
  { return FALSE;
  }
  for (i = 0; i < hr_table->length; i++)
  { hr_table->slots[i] = NULL;
  }
  hr_table->count = 0;
  return TRUE;
}

/* an empty table of some default length is successfully created, where none was
 * before */
static bool
jni_hr_create_default()
{ return jni_hr_create(101);
}

/* ep must point to a chain of zero or more entries; they are freed */
static void
jni_hr_free_chain_entries(HrEntry *ep)
{ if (ep != NULL)
  { jni_hr_free_chain_entries(ep->next);
    free(ep);
  }
}

/* table t is emptied */
static void
jni_hr_free_table_chains(HrTable *t)
{ int index;

  for (index = 0; index < (t->length); index++)
  { jni_hr_free_chain_entries(t->slots[index]);
    t->slots[index] = NULL;
  }
  t->count = 0;
}

/* all dynamic space used by the pointed-to table is freed
 */
static bool
jni_hr_free_table(HrTable *t)
{ if (t == NULL)
  { return FALSE; /* table does not exist */
  } else
  { jni_hr_free_table_chains(t);
    free(t);
    return TRUE;
  }
}

/* the current table is replaced by an equivalent one with more free space
 */
static bool
jni_hr_rehash(void)
{ HrTable *t0;    /* old table while building new one from it */
  int      i;     /* for iterating through slots in old table */
  HrEntry *ep1;   /* for iterating through all entries in old table */
  HrEntry *ep2;   /* an old table entry being relinked into new table */
  int      index; /* slot index in new table of entry being transferred */

  t0       = hr_table; /* temporarily hold onto former table */
  hr_table = NULL;
  if (!jni_hr_create(2 * t0->length + 1))
  { hr_table = t0;
    return FALSE;  /* failed to create replacement table during rehash */
  }
  for (i = 0; i < t0->length; i++) /* for each slot in *former* table */
  { for (ep1 = t0->slots[i]; ep1 != NULL;)
    { ep2       = ep1;
      ep1       = ep1->next;
      index     = (ep2->hash & 0x7fffffff) % hr_table->length; /* new */
      ep2->next = hr_table->slots[index];
      hr_table->slots[index] = ep2;
    }
    t0->slots[i] = NULL;
  }
  hr_table->count = t0->count;
  jni_hr_free_table(t0);

  return TRUE;
}

/* obj:  MUST BE a valid non-null reference to a Java object
 * hash: gets obj's System.identityHashCode()
 */

static bool
jni_hr_hash(JNIEnv *env, jobject obj, int *hash)
{ jobject e;                            /* possible (but unlikely?) exception */

  *hash = (*env)->CallStaticIntMethod(env, sys_class, sys_ihc, obj, obj);
  return (e = (*env)->ExceptionOccurred(env)) == NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Add jobject `lref` to object proxy table, setting `iref` to the
integerised canonical global ref for `lref`.

returns
  JNI_HR_ADD_NEW  -> referenced object is novel
  JNI_HR_ADD_OLD  -> referenced object is already known
  JNI_HR_ADD_FAIL -> something went wrong
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define HASH_INDEX(table, hash) (((hash) & 0x7fffffff) % (table)->length)

static int
jni_hr_add_unlocked(JNIEnv * env, jobject  lref, pointer *iref)
{ int      hash;  /* System.identityHashCode of lref */
  int      index; /* lref's slot index, from hash */
  HrEntry *ep;    /* temp entry pointer for chain traversal */
  jobject  gref;  /* iff lref is novel, will hold a global surrogate */

  if (hr_table == NULL && !jni_hr_create_default())
  { return JNI_HR_ADD_FAIL; /* lazy table creation failed: oughta sort return
                               codes */
  }
  if (!jni_hr_hash(env, lref, &hash))
  { return JNI_HR_ADD_FAIL; /* System.identityHashCode() failed (?) */
  }
  index = HASH_INDEX(hr_table, hash);
  for (ep = hr_table->slots[index]; ep != NULL; ep = ep->next)
  { if (ep->hash == hash)
    { if ((*env)->IsSameObject(env, ep->obj, lref))
      { /* newly referenced object is already interned */
        (*env)->DeleteLocalRef(env, lref); /* free redundant new ref */
        *iref = (pointer)ep->obj;          /* old, equivalent (global) ref */
        return JNI_HR_ADD_OLD;
      }
    }
  }
  if (hr_table->count >= hr_table->threshold)
  { (void)jni_hr_rehash(); /* oughta check for failure, and return it... */
    index = HASH_INDEX(hr_table, hash);
  }
  /* referenced object is novel, and we can add it to table */
  if ( (gref = (*env)->NewGlobalRef(env, lref)) == NULL) /* derive a global ref */
  { return JNI_HR_ADD_FAIL;
  }
  (*env)->DeleteLocalRef(env, lref); /* free redundant (local) ref */
  ep       = (HrEntry *)malloc(sizeof(HrEntry));
  ep->hash = hash;
  ep->obj  = gref;
  ep->next = hr_table->slots[index]; /* insert at front of chain */
  hr_table->slots[index] = ep;
  hr_table->count++;
  *iref = (pointer)gref; /* pass back the (new) global ref */
  return JNI_HR_ADD_NEW; /* obj was newly interned, under iref as supplied */
}

/* iref corresponded to an entry in the current HashedRef table;
   now that entry is gone, its space is recovered, counts are adjusted
   etc. called only from jni_free_iref() iref: a possibly spurious
   canonical global iref
*/

static bool
jni_hr_del_unlocked(JNIEnv *env, pointer iref)
{ int       index; /* index to a HashedRef table slot */
  HrEntry * ep;    /* pointer to a HashedRef table entry */
  HrEntry **epp;   /* pointer to ep's handle, in case it needs updating */

  DEBUG(1, Sdprintf("[removing possible object reference %p]\n", iref));
  for (index = 0; index < hr_table->length; index++) /* for each slot */
  { for (epp = &(hr_table->slots[index]), ep = *epp; ep != NULL;
         epp = &(ep->next), ep = *epp)
    { if ((pointer)(ep->obj) == iref) /* found the sought entry? */
      { (*env)->DeleteGlobalRef(env,
                                ep->obj); /* free the global object reference */
        *epp = ep->next;                  /* bypass the entry */
        free(ep);                         /* free the now-redundant space */
        hr_table->count--;                /* adjust table's entry count */
        DEBUG(1,
              Sdprintf(
                  "[found & removed hashtable entry for object reference %p]\n",
                  iref));
        return TRUE; /* entry found and removed */
      }
    }
  }
  DEBUG(1, Sdprintf("[JPL: failed to find hashtable entry for (presumably "
                    "bogus) object reference %p]\n",
                    iref));
  return FALSE;
}

static int
jni_hr_add(JNIEnv * env, jobject  lref, pointer *iref)
{ int rc;

  pthread_mutex_lock(&jref_mutex);
  rc = jni_hr_add_unlocked(env, lref, iref);
  pthread_mutex_unlock(&jref_mutex);

  return rc;
}

static int
jni_hr_del(JNIEnv * env, pointer iref)
{ int rc;

  pthread_mutex_lock(&jref_mutex);
  rc = jni_hr_del_unlocked(env, iref);
  pthread_mutex_unlock(&jref_mutex);

  return rc;
}


/*=== JNI initialisation ================================================== */

/* called once: after successful PVM & JVM creation/discovery, before any JNI
 * calls */
static int
jni_init(void)
{ jclass  lref;            /* temporary local ref, replaced by global */
  JNIEnv *env = jni_env(); /* could pass this in, but this is easier here */

  if (env == NULL)
    return -8;

  /* these initialisations require an active PVM: */
  JNI_atom_false = PL_new_atom("false");
  JNI_atom_true  = PL_new_atom("true");

  JNI_atom_boolean = PL_new_atom("boolean");
  JNI_atom_char    = PL_new_atom("char");
  JNI_atom_byte    = PL_new_atom("byte");
  JNI_atom_short   = PL_new_atom("short");
  JNI_atom_int     = PL_new_atom("int");
  JNI_atom_long    = PL_new_atom("long");
  JNI_atom_float   = PL_new_atom("float");
  JNI_atom_double  = PL_new_atom("double");

  JNI_atom_null = PL_new_atom("null");
  JNI_atom_void = PL_new_atom("void"); /* not yet used properly (?) */

  JNI_functor_at_1             = PL_new_functor(PL_new_atom("@"), 1);
  JNI_functor_jbuf_2           = PL_new_functor(PL_new_atom("jbuf"), 2);
  JNI_functor_jlong_2          = PL_new_functor(PL_new_atom("jlong"), 2);
  JNI_functor_jfieldID_1       = PL_new_functor(PL_new_atom("jfieldID"), 1);
  JNI_functor_jmethodID_1      = PL_new_functor(PL_new_atom("jmethodID"), 1);
  JNI_functor_error_2          = PL_new_functor(PL_new_atom("error"), 2);
  JNI_functor_java_exception_1 = PL_new_functor(PL_new_atom("java_exception"), 1);
  JNI_functor_jpl_error_1      = PL_new_functor(PL_new_atom("jpl_error"), 1);
  JNI_functor_pair_2           = PL_new_functor(PL_new_atom("-"), 2);
  (*env)->ExceptionClear(env);
  /* these initialisations require an active JVM: */
  return (
      (lref = (*env)->FindClass(env, "java/lang/Class")) != NULL &&
              (c_class = (*env)->NewGlobalRef(env, lref)) != NULL &&
              ((*env)->DeleteLocalRef(env, lref), TRUE)

              && (lref = (*env)->FindClass(env, "java/lang/String")) != NULL &&
              (str_class = (*env)->NewGlobalRef(env, lref)) != NULL &&
              ((*env)->DeleteLocalRef(env, lref), TRUE) &&
              (c_getName = (*env)->GetMethodID(env, c_class, "getName",
                                               "()Ljava/lang/String;")) != NULL

              && (lref = (*env)->FindClass(env, "java/lang/System")) != NULL &&
              (sys_class = (*env)->NewGlobalRef(env, lref)) != NULL &&
              ((*env)->DeleteLocalRef(env, lref), TRUE) &&
              (sys_ihc =
                   (*env)->GetStaticMethodID(env, sys_class, "identityHashCode",
                                             "(Ljava/lang/Object;)I")) != NULL

              && (lref = (*env)->FindClass(env, "org/jpl7/Term")) != NULL &&
              (term_class = (*env)->NewGlobalRef(env, lref)) != NULL &&
              ((*env)->DeleteLocalRef(env, lref), TRUE) &&
              (term_getTerm = (*env)->GetStaticMethodID(
                   env, term_class, "getTerm",
                   "(Lorg/jpl7/fli/term_t;)Lorg/jpl7/Term;")) != NULL &&
              (term_put = (*env)->GetMethodID(env, term_class, "put",
                                              "(Lorg/jpl7/fli/term_t;)V")) !=
                  NULL &&
              (term_putTerm = (*env)->GetStaticMethodID(
                   env, term_class, "putTerm",
                   "(Ljava/lang/Object;Lorg/jpl7/fli/term_t;)V")) != NULL

              &&
              (lref = (*env)->FindClass(env, "org/jpl7/fli/term_t")) != NULL &&
              (termt_class = (*env)->NewGlobalRef(env, lref)) != NULL &&
              ((*env)->DeleteLocalRef(env, lref), TRUE)

      ? (   (*env)->ExceptionClear(env), 0)
          : -7 /* NB #define this? */
      );
}

/*=== JNI exception/error processing/support ============================== */

/* returns a new error(java_exception(<jref>(hex)),msg) to represent a caught
 * Java exception */
static term_t
jni_new_java_exception(pointer i, atom_t msg)
{ term_t av;
  int    rc = ((av = PL_new_term_refs(2)) && jni_unify_iref(av + 1, i) &&
            PL_unify_term(av + 0, PL_FUNCTOR, JNI_functor_error_2, PL_FUNCTOR,
                          JNI_functor_java_exception_1,
                          /* PL_FUNCTOR, JNI_functor_at_1, */
                          PL_TERM, av + 1, PL_ATOM, msg));

  if (rc)
  { PL_reset_term_refs(av + 1);
    return av + 0;
  }

  return 0;
}

/* returns a new error(jpl_error(<jref>(hex)),msg) to represent an exceptional
 * condition raised within JPL */
static term_t
jni_new_jpl_error(const char *tag, pointer i)
{ term_t av;
  int    rc = ((av = PL_new_term_refs(2)) && jni_unify_iref(av + 1, i) &&
            PL_unify_term(av + 0, PL_FUNCTOR, JNI_functor_error_2, PL_FUNCTOR,
                          JNI_functor_jpl_error_1,
                          /*    PL_FUNCTOR, JNI_functor_at_1, */
                          PL_CHARS, tag, PL_TERM, av + 1));

  if (rc)
  { PL_reset_term_refs(av + 1);
    return av + 0;
  }

  return 0;
}

/* test for a raised exception; clear and report it if found */
static bool
jni_check_exception(JNIEnv *env)
{ jobject ej;  /* the pending Java exception, if any */
  jobject c;   /* its class */
  jobject s;   /* its class name as a JVM String, for the report */
  term_t  ep;  /* a newly created Prolog exception */
  pointer i;   /* temp for an iref denoting a Java exception */
  atom_t  msg; /* temp for impl-def comment (classname) within error/2 */

  if ((ej = (*env)->ExceptionOccurred(env)) == NULL)
  { return TRUE;
  } else
  { (*env)->ExceptionClear(
        env); /* clear "exception-pending" state so we can do JNI calls */
    if ((c = (*env)->GetObjectClass(env, ej)) !=
        NULL) /* get class of exception */
    { if ((s = (*env)->CallObjectMethod(env, c, c_getName)) !=
          NULL) /* get name of class */
      { if (jni_object_to_iref(env, ej, &i))
        { if (jni_String_to_atom(env, s, &msg))
          { ep = jni_new_java_exception(i, msg);
          } else
          { ep = jni_new_jpl_error(
                "FailedToGetUTFCharsOfNameOfClassOfException", i);
          }
        } else
        { ep = jni_new_jpl_error("FailedToConvertExceptionObjectToIref", 0);
        }
        (*env)->DeleteLocalRef(env, s);
      } else
      { ep = jni_new_jpl_error("FailedToGetNameOfClassOfException", 0);
      }
      (*env)->DeleteLocalRef(env, c);
    } else
    { ep = jni_new_jpl_error("FailedToGetClassOfException", 0);
    }
    return PL_raise_exception(ep);
  }
}

/*=== buffer and method param transput ==================================== */

static foreign_t
jni_byte_buf_length_to_codes_plc(term_t tbb,  /* +integer */
                                 term_t tlen, /* +integer */
                                 term_t tcs   /* -term */
                                 )
{ functor_t fn;
  term_t    a1;
  atom_t    a;
  term_t    a2;
  jbyte *   bb;
  int       len;
  int       i;
  term_t    tl = PL_copy_term_ref(tcs);
  term_t    ta = PL_new_term_ref();
  void *    ptr;

  if (!(PL_get_functor(tbb, &fn) && fn == JNI_functor_jbuf_2 &&
        (a2 = PL_new_term_ref(), PL_get_arg(2, tbb, a2)) &&
        PL_get_atom(a2, &a) && a == JNI_atom_byte &&
        (a1 = PL_new_term_ref(), PL_get_arg(1, tbb, a1)) &&
        PL_get_pointer(a1, &ptr)) ||
      !PL_get_integer(tlen, &len))
  { return FALSE;
  }
  bb = ptr;

  for (i = 0; i < len; i++)
  { if (!PL_unify_list(tl, ta, tl) || !PL_unify_integer(ta, (int)(bb[i])))
    { return FALSE;
    }
  }
  return PL_unify_nil(tl);
}

static foreign_t /* can a "user" mistake cause this to fail? */
    jni_param_put_plc(
        term_t tn,  /* +integer: index, as Prolog integer, of this method param
                       (0 -> first) */
        term_t txc, /* +integer: transput code, as Prolog integer, appropriate
                       to this param */
        term_t tt,  /* +term: param value as datum (value or ref) */
        term_t tjvp /* +pointer: param buffer (allocated just for this call) */
        )
{ int     n;   /* got from tn (see above) */
  int     xc;  /* got from txc (see above) */
  jvalue *jvp; /* got from tjvp (see above) */

  if (!PL_get_integer(tn, &n) || !PL_get_integer(txc, &xc) ||
      !PL_get_pointer(tjvp, (void *)&jvp))
  { return FALSE;
  }

  switch (xc)
  { case JNI_XPUT_BOOLEAN:
      return jni_term_to_jboolean(tt, &jvp[n].z);

    case JNI_XPUT_BYTE:
      return jni_term_to_jbyte(tt, &jvp[n].b);

    case JNI_XPUT_CHAR:
      return jni_term_to_jchar(tt, &jvp[n].c);

    case JNI_XPUT_SHORT:
      return jni_term_to_jshort(tt, &jvp[n].s);

    case JNI_XPUT_INT:
      return jni_term_to_jint(tt, &jvp[n].i);

    case JNI_XPUT_LONG:
      return jni_term_to_jlong(tt, &jvp[n].j);

    case JNI_XPUT_FLOAT:
      return jni_term_to_jfloat(tt, &jvp[n].f);

    case JNI_XPUT_DOUBLE:
      return jni_term_to_jdouble(tt, &jvp[n].d);

    case JNI_XPUT_REF:
    { JNIEnv *env = jni_env();

      return env == NULL ? FALSE : jni_term_to_ref(tt, &jvp[n].l, env);
    }
    default:
      return FALSE; /* unknown or inappropriate JNI_XPUT_* code */
  }
}

/* for completeness, allocates zero-length buffers too, while avoiding malloc()
 * problems */
static foreign_t
jni_alloc_buffer_plc(
    term_t txc,  /* +integer: transput code */
    term_t tlen, /* +integer: required length (# items) */
    term_t tbp   /* -pointer: PL_POINTER to newly allocated buffer */
    )
{ int   xc;
  int   len;
  void *bp;

  return PL_get_integer(txc, &xc) &&
         ((xc >= JNI_XPUT_BOOLEAN && xc <= JNI_XPUT_DOUBLE) ||
          xc == JNI_XPUT_JVALUE) &&
         PL_get_integer(tlen, &len) && len >= 0 &&
         (bp = malloc((len == 0 ? 1 : len) * size[xc])) !=
             NULL /* avoid (unsafe) malloc(0) */
         && (PL_unify_pointer(tbp, (void *)bp) ? TRUE : (free(bp), FALSE));
}

static foreign_t
jni_free_buffer_plc(term_t tbp /* +integer: PL_POINTER to redundant buffer */
                    )
{ void *bp;

  return PL_get_pointer(tbp, &bp) && (free(bp), TRUE);
}

static foreign_t
jni_fetch_buffer_value_plc(
    term_t tbp, /* +pointer: PL_POINTER to an active buffer from
                   jni_alloc_buffer/3 */
    term_t ti,  /* +integer: index into buffer; 0 <= i < length */
    term_t tv,  /* -term: required value (@(false), @(true), integer or float)
                   from buffer */
    term_t txc  /* +integer: transput code (one of JNI_XPUT_*) */
    )
{ void *bp; /* buffer address (trusted to be valid) */
  int   i;  /* buffer index (trusted to be valid) */
  int   xc; /* transput code (range-checked by switch statement) */

  if (!PL_get_pointer(tbp, &bp) || !PL_get_integer(ti, &i) ||
      !PL_get_integer(txc, &xc))
  { return FALSE;
  }

  switch (xc) /* primitive type only */
  { case JNI_XPUT_BOOLEAN:
      return jni_jboolean_to_term(((jboolean *)bp)[i], tv);

    case JNI_XPUT_CHAR:
      return PL_unify_integer(tv, ((jchar *)bp)[i]);

    case JNI_XPUT_BYTE:
      return PL_unify_integer(tv, ((jbyte *)bp)[i]);

    case JNI_XPUT_SHORT:
      return PL_unify_integer(tv, ((jshort *)bp)[i]);

    case JNI_XPUT_INT:
      return PL_unify_integer(tv, ((jint *)bp)[i]);

    case JNI_XPUT_LONG:
      return PL_unify_int64(tv, ((jlong *)bp)[i]);

    case JNI_XPUT_FLOAT:
      return PL_unify_float(tv, ((jfloat *)bp)[i]);

    case JNI_XPUT_DOUBLE:
      return PL_unify_float(tv, ((jdouble *)bp)[i]);

    default:
      return FALSE;
  }
}

static foreign_t
jni_stash_buffer_value_plc(
    term_t tbp, /* +integer: PL_POINTER to buffer */
    term_t ti,  /* +integer: index into buffer */
    term_t tv,  /* +term: @(false), @(true), integer or float */
    term_t txc  /* +integer: transput code (one of JNI_XPUT_*) */
    )
{ void *bp;
  int   idx;
  int   xc;

  if (!PL_get_pointer(tbp, &bp) || !PL_get_integer(ti, &idx) ||
      !PL_get_integer(txc, &xc))
  { return FALSE;
  }

  switch (xc)
  { case JNI_XPUT_BOOLEAN:
      return jni_term_to_jboolean(tv, &((jboolean *)bp)[idx]);

    case JNI_XPUT_CHAR:
      return jni_term_to_jchar(tv, &((jchar *)bp)[idx]);

    case JNI_XPUT_BYTE:
      return jni_term_to_jbyte(tv, &((jbyte *)bp)[idx]);

    case JNI_XPUT_SHORT:
      return jni_term_to_jshort(tv, &((jshort *)bp)[idx]);

    case JNI_XPUT_INT:
      return jni_term_to_jint(tv, &((jint *)bp)[idx]);

    case JNI_XPUT_LONG:
      return jni_term_to_jlong(tv, &((jlong *)bp)[idx]);

    case JNI_XPUT_FLOAT:
      return jni_term_to_jfloat(tv, &((jfloat *)bp)[idx]);

    case JNI_XPUT_DOUBLE:
      return jni_term_to_jdouble(tv, &((jdouble *)bp)[idx]);

    default:
      return FALSE;
  }
}

/*=== JVM initialisation, startup etc. ==================================== */

static int
jni_get_created_jvm_count()
{ jint n;

  return (JNI_GetCreatedJavaVMs(NULL, 0, &n) ==
                  0 /* what does the '0' arg mean? */
              ? n
              : -1);
}

#ifdef __linux__
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Work around a temporary Linux issue.  See [1]

https://stackoverflow.com/questions/44763387/jni-createjavavm-stack-corruption-in-recent-ubuntu-16-04
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
fix_thread_stack(JavaVMOption *opt, int optn)
{ int i;

  for(i=0; i<optn; i++)
  { if ( strncmp(opt[i].optionString, "-Xss", 3) == 0 )
      break;
  }
  if ( i == optn )
    opt[optn++].optionString = "-Xss1280k";

  return optn;
}
#endif

#define MAX_JVM_OPTIONS 100

static int
jni_create_jvm_c(char *classpath)
{ JavaVMInitArgs vm_args;
  /*    char                            cpopt[10000]; */
  char *       cpoptp;
  JavaVMOption opt[MAX_JVM_OPTIONS];
  int          r;
  jint         n = 0;
  int          optn = 0;
  JNIEnv *     env;

  DEBUG(1, Sdprintf("[creating JVM with 'java.class.path=%s']\n", classpath));
  vm_args.version = JNI_VERSION_1_2; /* "Java 1.2 please" */
  if (classpath)
    { cpoptp = (char *)calloc(1,strlen(classpath) + 20);
    strcpy(cpoptp, "-Djava.class.path="); /* was cpopt */
    strcat(cpoptp, classpath);            /* oughta check length... */
    vm_args.options        = opt;
    opt[optn].optionString = cpoptp; /* was cpopt */
    optn++;
  }
  /* opt[optn++].optionString = "-Djava.compiler=NONE"; */
  /* opt[optn].optionString = "exit";       // I don't understand this yet... */
  /* opt[optn++].extraInfo = jvm_exit;          // this function has been moved to
   * jpl_extras.c */
  /* opt[optn].optionString = "abort";      // I don't understand this yet... */
  /* opt[optn++].extraInfo = jvm_abort;         // this function has been moved
   * to jpl_extras.c */
  /* opt[optn++].optionString = "-Xcheck:jni";    // extra checking of JNI calls
   */
  /* opt[optn++].optionString = "-Xnoclassgc";    // so method/field IDs remain
   * valid (?) */
  /* opt[optn].optionString = "vfprintf"; */
  /* opt[optn++].extraInfo = fprintf;               // no O/P, then SEGV */
  /* opt[optn++].extraInfo = xprintf;               // one message, then SEGV */
  /* opt[optn++].optionString = "-verbose:jni"; */
  vm_args.ignoreUnrecognized = false;
  if (jvm_dia != NULL)
  { int i;

    for (i = 0; jvm_dia[i] != NULL; i++)
    {
      opt[optn].extraInfo = NULL;
       opt[optn++].optionString = jvm_dia[i];
    }
    jvm_aia = jvm_dia;
    jvm_dia = NULL;
  }

#ifdef __linux__
  optn = fix_thread_stack(opt, optn);
#endif

  vm_args.nOptions = optn;
  /* vm_args.ignoreUnrecognized = TRUE; */

  return ( (JNI_GetCreatedJavaVMs(&jvm, sizeof(*jvm), &n) ==
		   0) /* what does the '1' arg mean? */
                  && n == 1
                  /* && (*jvm)->GetEnv(jvm,(void**)&env,JNI_VERSION_1_2) ==
                     JNI_OK */
                  && (env = jni_env()) != NULL
              ? 2 /* success (JVM already available) */
              : ((r = JNI_CreateJavaVM(&jvm, (void **)&env, &vm_args)) == 0
		 ? 0             /* success (JVM created OK) */
                     : (jvm = NULL ,r) /* -ve, i.e. some create error */
                 ));
}

static foreign_t
jni_get_created_jvm_count_plc(term_t t1)
{ return PL_unify_integer(t1, jni_get_created_jvm_count());
}

static int
jni_create_jvm(char *cp)
{ int r1;
  int r2;

  DEBUG(1, Sdprintf("[JPL: checking for Java VM...]\n"));
  return (
      jvm != NULL
          ? 1 /* already initialised */
          : ((r1 = jni_create_jvm_c(cp)) < 0
                 ? r1 /* err code from JVM-specific routine */
                 : ((r2 = jni_init()) < 0
                        ? r2       /* err code from jni_init() */
                        : (r1 == 0 /* success code from JVM-specific routine */
                               ? (DEBUG(0,
                                        Sdprintf("[JPL: Java VM created]\n")),
                                  r1)
                               : (DEBUG(0, Sdprintf("[JPL: Java VM found]\n")),
                                  r1)))));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
JW: Note: recent SWI-Prolog set the environment  using Win32 API. We can
only get the proper value using the   Win32 API; getenv only returns the
value at startup of Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
jni_create_default_jvm()
{ int r;
#ifdef __WINDOWS__
  char *cp;
  DWORD len;

  if ((len = GetEnvironmentVariable("CLASSPATH", NULL, 0)) > 0)
  { cp = malloc(len + 1);

    GetEnvironmentVariable("CLASSPATH", cp, len + 1);
  } else
    cp = NULL;
#else
  char *cp = getenv("CLASSPATH");
#endif

  DEBUG(3, Sdprintf("jni_create_default_jvm(): cp=%s\n", cp));
   if ((r = jni_create_jvm(cp)) < 0)
  { Sdprintf("[JPL: failed to create Java VM (error %d)]\n", r);
  }
  
  return r >= 0; /* e.g. 2 -> "JVM already available" */
}

static foreign_t
jni_ensure_jvm_plc()
{ JNIEnv
      *env; /* not used but perhaps initialised by the jni_ensure_jvm() macro */

  return jni_ensure_jvm();
}

/* NB after any JNI call which clearly indicates success, */
/* it is unnecessary to check for an exception */
/* (potential for slight economy here...) */
static foreign_t
jni_void_0_plc(/* C identifiers distinguished _0_ etc, Prolog name is overloaded
                  */
               term_t tn /* +integer */
               )
{ int      n; /* JNI function index */
  jboolean r; /* Prolog exit/fail outcome */
  JNIEnv * env;

  if (!jni_ensure_jvm() /* ought this either succeed or throw a JPL error? */
      ||
      !PL_get_integer(
          tn, &n) /* ought this either succeed or throw a Prolog type error? */
      )
  { return FALSE;
  }

  switch (n)
  { case 17:
      r = ((*env)->ExceptionClear(env), TRUE); /* could just return... */
      break;
    default:
      return FALSE; /* oughta throw exception (design-time error :-) */
      break;
  }

  return jni_check_exception(env) && r;
}

static foreign_t
jni_void_1_plc(term_t tn, /* +integer: JNI function index */
               term_t ta1 /* +term: Arg1 */
               )
{ int      n;  /* JNI function index */
  char *   c1; /*  " */
  jboolean r;  /* Prolog exit/fail outcome */
  JNIEnv * env;

  if (!jni_ensure_jvm() || !PL_get_integer(tn, &n))
  { return FALSE;
  }

  switch (n)
  { case 18:
      r = jni_term_to_charP(ta1, &c1) && ((*env)->FatalError(env, c1), TRUE);
      break;
    default:
      return FALSE; /* oughta throw exception (design-time error :-) */
      break;
  }

  return jni_check_exception(env) && r;
}

static foreign_t
jni_void_2_plc(term_t tn,  /* +integer: JNI function index */
               term_t ta1, /* +term: Arg1 */
               term_t ta2  /* +term: Arg2 */
               )
{ int      n; /* JNI function index */
  jboolean r; /* Prolog exit/fail outcome */
  JNIEnv * env;

  if (!jni_ensure_jvm() || !PL_get_integer(tn, &n))
  { return FALSE;
  }

  switch (n)
  { default:
      return FALSE; /* oughta throw exception (design-time error :-) */
      break;
  }

  return jni_check_exception(env) && r;
}

static foreign_t
jni_void_3_plc(term_t tn,  /* +integer: JNI function index */
               term_t ta1, /* +term: Arg1 */
               term_t ta2, /* +term: Arg2 */
               term_t ta3  /* +term: Arg3 */
               )
{ int          n; /* JNI function index */
  jobject      jo1;
  jobject      jo3;
  jclass       jc1;
  jfieldID     jfi2;
  jmethodID    jmi2;
  jobjectArray oa1;
  jchar        jc3;
  jbyte        jb3;
  jshort       js3;
  jint         ji3;
  jsize        js2;
  jlong        jl3;
  jfloat       jf3;
  jdouble      jd3;
  jboolean     jz3;
  jvalue *     jvp3 =
      NULL;   /* if this is given a buffer, it will be freed after the call */
  jboolean r; /* Prolog exit/fail outcome */
  JNIEnv * env;

  if (!jni_ensure_jvm() || !PL_get_integer(tn, &n))
  { return FALSE;
  }

  switch (n)
  { case 63:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          ((*env)->CallVoidMethodA(env, jo1, jmi2, jvp3), TRUE);
      break;
    case 104:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_ref(ta3, &jo3, env) &&
          ((*env)->SetObjectField(env, jo1, jfi2, jo3), TRUE);
      break;
    case 105:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_jboolean(ta3, &jz3) &&
          ((*env)->SetBooleanField(env, jo1, jfi2, jz3), TRUE);
      break;
    case 106:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_jbyte(ta3, &jb3) &&
          ((*env)->SetByteField(env, jo1, jfi2, jb3), TRUE);
      break;
    case 107:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_jchar(ta3, &jc3) &&
          ((*env)->SetCharField(env, jo1, jfi2, jc3), TRUE);
      break;
    case 108:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_jshort(ta3, &js3) &&
          ((*env)->SetShortField(env, jo1, jfi2, js3), TRUE);
      break;
    case 109:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_jint(ta3, &ji3) &&
          ((*env)->SetIntField(env, jo1, jfi2, ji3), TRUE);
      break;
    case 110:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_jlong(ta3, &jl3) &&
          ((*env)->SetLongField(env, jo1, jfi2, jl3), TRUE);
      break;
    case 111:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_jfloat(ta3, &jf3) &&
          ((*env)->SetFloatField(env, jo1, jfi2, jf3), TRUE);
      break;
    case 112:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_jdouble(ta3, &jd3) &&
          ((*env)->SetDoubleField(env, jo1, jfi2, jd3), TRUE);
      break;
    case 143:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          ((*env)->CallStaticVoidMethodA(env, jc1, jmi2, jvp3), TRUE);
      break;
    case 154:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_ref(ta3, &jo3, env) &&
          ((*env)->SetStaticObjectField(env, jc1, jfi2, jo3), TRUE);
      break;
    case 155:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_jboolean(ta3, &jz3) &&
          ((*env)->SetStaticBooleanField(env, jc1, jfi2, jz3), TRUE);
      break;
    case 156:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_jbyte(ta3, &jb3) &&
          ((*env)->SetStaticByteField(env, jc1, jfi2, jb3), TRUE);
      break;
    case 157:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_jchar(ta3, &jc3) &&
          ((*env)->SetStaticCharField(env, jc1, jfi2, jc3), TRUE);
      break;
    case 158:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_jshort(ta3, &js3) &&
          ((*env)->SetStaticShortField(env, jc1, jfi2, js3), TRUE);
      break;
    case 159:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_jint(ta3, &ji3) &&
          ((*env)->SetStaticIntField(env, jc1, jfi2, ji3), TRUE);
      break;
    case 160:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_jlong(ta3, &jl3) &&
          ((*env)->SetStaticLongField(env, jc1, jfi2, jl3), TRUE);
      break;
    case 161:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_jfloat(ta3, &jf3) &&
          ((*env)->SetStaticFloatField(env, jc1, jfi2, jf3), TRUE);
      break;
    case 162:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) && jni_term_to_jdouble(ta3, &jd3) &&
          ((*env)->SetStaticDoubleField(env, jc1, jfi2, jd3), TRUE);
      break;
    case 174:
      r = jni_term_to_object_jarray(ta1, &oa1, env) &&
          jni_term_to_jsize(ta2, &js2) && jni_term_to_ref(ta3, &jo3, env) &&
          ((*env)->SetObjectArrayElement(env, oa1, js2, jo3), TRUE);
      break;
    default:
      return FALSE; /* oughta throw exception (design-time error :-) */
      break;
  }

  if (jvp3 != NULL)
  { free(jvp3);
  }

  return jni_check_exception(env) && r;
}

static foreign_t
jni_void_4_plc(term_t tn,  /* +integer: JNI function index */
               term_t ta1, /* +term: Arg1 */
               term_t ta2, /* +term: Arg2 */
               term_t ta3, /* +term: Arg3 */
               term_t ta4  /* +term: Arg4 */
               )
{ int           n; /* JNI function index */
  jbooleanArray za1;
  jbyteArray    ba1;
  jcharArray    ca1;
  jshortArray   sa1;
  jintArray     ia1;
  jlongArray    la1;
  jfloatArray   fa1;
  jdoubleArray  da1;
  jsize         js2;
  jsize         js3;
  jboolean *    jzp4;
  jbyte *       jbp4;
  jchar *       jcp4;
  jshort *      jsp4;
  jint *        jip4;
  jlong *       jlp4;
  jfloat *      jfp4;
  jdouble *     jdp4;
  jvalue *      jvp3 =
      NULL;   /* if this is given a buffer, it will be freed after the call */
  jboolean r; /* Prolog exit/fail outcome */
  JNIEnv * env;

  if (!jni_ensure_jvm() || !PL_get_integer(tn, &n))
  { return FALSE;
  }

  switch (n)
  { case 199:
      r = jni_term_to_boolean_jarray(ta1, &za1, env) &&
          jni_term_to_jsize(ta2, &js2) && jni_term_to_jint(ta3, &js3) &&
          jni_term_to_jboolean_buf(ta4, &jzp4) &&
          ((*env)->GetBooleanArrayRegion(env, za1, js2, js3, jzp4), TRUE);
      break;
    case 200:
      r = jni_term_to_byte_jarray(ta1, &ba1, env) &&
          jni_term_to_jsize(ta2, &js2) && jni_term_to_jsize(ta3, &js3) &&
          jni_term_to_jbyte_buf(ta4, &jbp4) &&
          ((*env)->GetByteArrayRegion(env, ba1, js2, js3, jbp4), TRUE);
      break;
    case 201:
      r = jni_term_to_char_jarray(ta1, &ca1, env) &&
          jni_term_to_jsize(ta2, &js2) && jni_term_to_jsize(ta3, &js3) &&
          jni_term_to_jchar_buf(ta4, &jcp4) &&
          ((*env)->GetCharArrayRegion(env, ca1, js2, js3, jcp4), TRUE);
      break;
    case 202:
      r = jni_term_to_short_jarray(ta1, &sa1, env) &&
          jni_term_to_jsize(ta2, &js2) && jni_term_to_jsize(ta3, &js3) &&
          jni_term_to_jshort_buf(ta4, &jsp4) &&
          ((*env)->GetShortArrayRegion(env, sa1, js2, js3, jsp4), TRUE);
      break;
    case 203:
      r = jni_term_to_int_jarray(ta1, &ia1, env) &&
          jni_term_to_jsize(ta2, &js2) && jni_term_to_jsize(ta3, &js3) &&
          jni_term_to_jint_buf(ta4, &jip4) &&
          ((*env)->GetIntArrayRegion(env, ia1, js2, js3, jip4), TRUE);
      break;
    case 204:
      r = jni_term_to_long_jarray(ta1, &la1, env) &&
          jni_term_to_jsize(ta2, &js2) && jni_term_to_jsize(ta3, &js3) &&
          jni_term_to_jlong_buf(ta4, &jlp4) &&
          ((*env)->GetLongArrayRegion(env, la1, js2, js3, jlp4), TRUE);
      break;
    case 205:
      r = jni_term_to_float_jarray(ta1, &fa1, env) &&
          jni_term_to_jsize(ta2, &js2) && jni_term_to_jsize(ta3, &js3) &&
          jni_term_to_jfloat_buf(ta4, &jfp4) &&
          ((*env)->GetFloatArrayRegion(env, fa1, js2, js3, jfp4), TRUE);
      break;
    case 206:
      r = jni_term_to_double_jarray(ta1, &da1, env) &&
          jni_term_to_jsize(ta2, &js2) && jni_term_to_jsize(ta3, &js3) &&
          jni_term_to_jdouble_buf(ta4, &jdp4) &&
          ((*env)->GetDoubleArrayRegion(env, da1, js2, js3, jdp4), TRUE);
      break;
    case 207:
      r = jni_term_to_boolean_jarray(ta1, &za1, env) &&
          jni_term_to_jsize(ta2, &js2) && jni_term_to_jsize(ta3, &js3) &&
          jni_term_to_jboolean_buf(ta4, &jzp4) &&
          ((*env)->SetBooleanArrayRegion(env, za1, js2, js3, jzp4), TRUE);
      break;
    case 208:
      r = jni_term_to_byte_jarray(ta1, &ba1, env) &&
          jni_term_to_jsize(ta2, &js2) && jni_term_to_jsize(ta3, &js3) &&
          jni_term_to_jbyte_buf(ta4, &jbp4) &&
          ((*env)->SetByteArrayRegion(env, ba1, js2, js3, jbp4), TRUE);
      break;
    case 209:
      r = jni_term_to_char_jarray(ta1, &ca1, env) &&
          jni_term_to_jsize(ta2, &js2) && jni_term_to_jsize(ta3, &js3) &&
          jni_term_to_jchar_buf(ta4, &jcp4) &&
          ((*env)->SetCharArrayRegion(env, ca1, js2, js3, jcp4), TRUE);
      break;
    case 210:
      r = jni_term_to_short_jarray(ta1, &sa1, env) &&
          jni_term_to_jsize(ta2, &js2) && jni_term_to_jsize(ta3, &js3) &&
          jni_term_to_jshort_buf(ta4, &jsp4) &&
          ((*env)->SetShortArrayRegion(env, sa1, js2, js3, jsp4), TRUE);
      break;
    case 211:
      r = jni_term_to_int_jarray(ta1, &ia1, env) &&
          jni_term_to_jsize(ta2, &js2) && jni_term_to_jsize(ta3, &js3) &&
          jni_term_to_jint_buf(ta4, &jip4) &&
          ((*env)->SetIntArrayRegion(env, ia1, js2, js3, jip4), TRUE);
      break;
    case 212:
      r = jni_term_to_long_jarray(ta1, &la1, env) &&
          jni_term_to_jsize(ta2, &js2) && jni_term_to_jsize(ta3, &js3) &&
          jni_term_to_jlong_buf(ta4, &jlp4) &&
          ((*env)->SetLongArrayRegion(env, la1, js2, js3, jlp4), TRUE);
      break;
    case 213:
      r = jni_term_to_float_jarray(ta1, &fa1, env) &&
          jni_term_to_jsize(ta2, &js2) && jni_term_to_jsize(ta3, &js3) &&
          jni_term_to_jfloat_buf(ta4, &jfp4) &&
          ((*env)->SetFloatArrayRegion(env, fa1, js2, js3, jfp4), TRUE);
      break;
    case 214:
      r = jni_term_to_double_jarray(ta1, &da1, env) &&
          jni_term_to_jsize(ta2, &js2) && jni_term_to_jsize(ta3, &js3) &&
          jni_term_to_jdouble_buf(ta4, &jdp4) &&
          ((*env)->SetDoubleArrayRegion(env, da1, js2, js3, jdp4), TRUE);
      break;
    default:
      return FALSE; /* oughta throw exception (design-time error :-) */
      break;
  }

  if (jvp3 != NULL)
  { free(jvp3);
  }

  return jni_check_exception(env) && r;
}

static foreign_t
jni_func_0_plc(term_t tn, /* +integer: JNI function index */
               term_t tr  /* -term: Result */
               )
{ int      n; /* JNI function index */
  jboolean r; /* Prolog exit/fail outcome */
  JNIEnv * env;

  if (!jni_ensure_jvm() || !PL_get_integer(tn, &n))
  { return FALSE;
  }

  switch (n)
  { default:
      return FALSE; /* oughta throw exception (design-time error :-) */
      break;
  }

  return jni_check_exception(env) &&
         r; /* surely NEITHER of these throws an exception! */
}

static foreign_t
jni_func_1_plc(term_t tn,  /* +integer: JNI function index */
               term_t ta1, /* +term: Arg1 */
               term_t tr   /* -term: Result */
               )
{ int      n;   /* JNI function index */
  char *   ch1; /*  " */
  jobject  jo1;
  jclass   jc1;
  jarray   ja1;
  jsize    js1;
  jboolean r; /* Prolog exit/fail outcome */
  JNIEnv * env;

  if (!jni_ensure_jvm() || !PL_get_integer(tn, &n))
  { return FALSE;
  }

  switch (n)
  { case 6:
      r = jni_term_to_charP(ta1, &ch1) &&
          jni_jobject_to_term((*env)->FindClass(env, ch1), tr,
                              env); /* *NOT* Unicode */
      break;
    case 10:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_jobject_to_term((*env)->GetSuperclass(env, jc1), tr, env);
      break;
    case 31:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_jobject_to_term((*env)->GetObjectClass(env, jo1), tr, env);
      break;
    case 171:
      r = jni_term_to_jarray(ta1, &ja1, env) &&
          jni_jint_to_term((*env)->GetArrayLength(env, ja1), tr);
      break;
    case 175:
      r = jni_term_to_non_neg_jsize(ta1, &js1) &&
          jni_jobject_to_term((*env)->NewBooleanArray(env, js1), tr, env);
      break;
    case 176:
      r = jni_term_to_non_neg_jsize(ta1, &js1) &&
          jni_jobject_to_term((*env)->NewByteArray(env, js1), tr, env);
      break;
    case 177:
      r = jni_term_to_non_neg_jsize(ta1, &js1) &&
          jni_jobject_to_term((*env)->NewCharArray(env, js1), tr, env);
      break;
    case 178:
      r = jni_term_to_non_neg_jsize(ta1, &js1) &&
          jni_jobject_to_term((*env)->NewShortArray(env, js1), tr, env);
      break;
    case 179:
      r = jni_term_to_non_neg_jsize(ta1, &js1) &&
          jni_jobject_to_term((*env)->NewIntArray(env, js1), tr, env);
      break;
    case 180:
      r = jni_term_to_non_neg_jsize(ta1, &js1) &&
          jni_jobject_to_term((*env)->NewLongArray(env, js1), tr, env);
      break;
    case 181:
      r = jni_term_to_non_neg_jsize(ta1, &js1) &&
          jni_jobject_to_term((*env)->NewFloatArray(env, js1), tr, env);
      break;
    case 182:
      r = jni_term_to_non_neg_jsize(ta1, &js1) &&
          jni_jobject_to_term((*env)->NewDoubleArray(env, js1), tr, env);
      break;
    default:
      return FALSE; /* oughta throw exception (design-time error :-) */
      break;
  }

  return jni_check_exception(env) && r;
}

static foreign_t
jni_func_2_plc(term_t tn,  /* +integer: JNI function index */
               term_t ta1, /* +term: Arg1 */
               term_t ta2, /* +term: Arg2 */
               term_t tr   /* -term: Result */
               )
{ int          n; /* JNI function index */
  jobject      jo1;
  jclass       jc1;
  jclass       c2;
  jobjectArray oa1;
  jsize        js2;
  jfieldID     jfi2;
  jboolean     r; /* Prolog exit/fail outcome */
  JNIEnv *     env;

  if (!jni_ensure_jvm() || !PL_get_integer(tn, &n))
  { return FALSE;
  }

  switch (n)
  { case 11:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jclass(ta2, &c2, env) &&
          jni_jboolean_to_term((*env)->IsAssignableFrom(env, jc1, c2), tr);
      break;
    case 95:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jobject_to_term((*env)->GetObjectField(env, jo1, jfi2), tr, env);
      break;
    case 96:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jboolean_to_term((*env)->GetBooleanField(env, jo1, jfi2), tr);
      break;
    case 97:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jbyte_to_term((*env)->GetByteField(env, jo1, jfi2), tr);
      break;
    case 98:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jchar_to_term((*env)->GetCharField(env, jo1, jfi2), tr);
      break;
    case 99:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jshort_to_term((*env)->GetShortField(env, jo1, jfi2), tr);
      break;
    case 100:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jint_to_term((*env)->GetIntField(env, jo1, jfi2), tr);
      break;
    case 101:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jlong_to_term((*env)->GetLongField(env, jo1, jfi2), tr);
      break;
    case 102:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jfloat_to_term((*env)->GetFloatField(env, jo1, jfi2), tr);
      break;
    case 103:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jdouble_to_term((*env)->GetDoubleField(env, jo1, jfi2), tr);
      break;
    case 145:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jobject_to_term((*env)->GetStaticObjectField(env, jc1, jfi2), tr,
                              env);
      break;
    case 146:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jboolean_to_term((*env)->GetStaticBooleanField(env, jc1, jfi2),
                               tr);
      break;
    case 147:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jbyte_to_term((*env)->GetStaticByteField(env, jc1, jfi2), tr);
      break;
    case 148:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jchar_to_term((*env)->GetStaticCharField(env, jc1, jfi2), tr);
      break;
    case 149:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jshort_to_term((*env)->GetStaticShortField(env, jc1, jfi2), tr);
      break;
    case 150:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jint_to_term((*env)->GetStaticIntField(env, jc1, jfi2), tr);
      break;
    case 151:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jlong_to_term((*env)->GetStaticLongField(env, jc1, jfi2), tr);
      break;
    case 152:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jfloat_to_term((*env)->GetStaticFloatField(env, jc1, jfi2), tr);
      break;
    case 153:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jfieldID(ta2, &jfi2) &&
          jni_jdouble_to_term((*env)->GetStaticDoubleField(env, jc1, jfi2), tr);
      break;
    case 173:
    { /* int i; */ /* JW: i is long in this function */

      js2 = 0; /* JW: make compiler happy (was i2) */
      r   = jni_term_to_object_jarray(ta1, &oa1, env) &&
          jni_term_to_jsize(ta2, &js2);
    }
      if (r)
        r = jni_jobject_to_term((*env)->GetObjectArrayElement(env, oa1, js2),
                                tr, env);
      break;
    default:
      return FALSE; /* oughta throw exception (design-time error :-) */
      break;
  }

  return jni_check_exception(env) && r;
}

static foreign_t
jni_func_3_plc(term_t tn,  /* +integer: JNI function index */
               term_t ta1, /* +term: Arg1 */
               term_t ta2, /* +term: Arg2 */
               term_t ta3, /* +term: Arg3 */
               term_t tr   /* -term: Result */
               )
{ int       n;   /* JNI function index */
  char *    ch2; /*  " */
  char *    ch3; /*  " */
  jclass    jc1;
  jclass    c2;
  jobject   jo1;
  jobject   jo3;
  jmethodID jmi2;
  jsize     js1;
  jvalue *  jvp3 =
      NULL;   /* if this is given a buffer, it will be freed after the call */
  jboolean r; /* Prolog exit/fail outcome */
  JNIEnv * env;

  if (!jni_ensure_jvm() || !PL_get_integer(tn, &n))
  { return FALSE;
  }

  switch (n)
  { case 30:
      r = jni_term_to_non_array_jclass(ta1, &jc1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jobject_to_term((*env)->NewObjectA(env, jc1, jmi2, jvp3), tr,
                              env);
      break;
    case 33:
      r = jni_term_to_jclass(ta1, &jc1, env) && jni_term_to_charP(ta2, &ch2) &&
          jni_term_to_charP(ta3, &ch3) &&
          jni_jmethodID_to_term((*env)->GetMethodID(env, jc1, ch2, ch3), tr);
      break;
    case 36:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jobject_to_term((*env)->CallObjectMethodA(env, jo1, jmi2, jvp3),
                              tr, env);
      break;
    case 39:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jboolean_to_term((*env)->CallBooleanMethodA(env, jo1, jmi2, jvp3),
                               tr);
      break;
    case 42:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jbyte_to_term((*env)->CallByteMethodA(env, jo1, jmi2, jvp3), tr);
      break;
    case 45:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jchar_to_term((*env)->CallCharMethodA(env, jo1, jmi2, jvp3), tr);
      break;
    case 48:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jshort_to_term((*env)->CallShortMethodA(env, jo1, jmi2, jvp3),
                             tr);
      break;
    case 51:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jint_to_term((*env)->CallIntMethodA(env, jo1, jmi2, jvp3), tr);
      break;
    case 54:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jlong_to_term((*env)->CallLongMethodA(env, jo1, jmi2, jvp3), tr);
      break;
    case 57:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jfloat_to_term((*env)->CallFloatMethodA(env, jo1, jmi2, jvp3),
                             tr);
      break;
    case 60:
      r = jni_term_to_jobject(ta1, &jo1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jdouble_to_term((*env)->CallDoubleMethodA(env, jo1, jmi2, jvp3),
                              tr);
      break;
    case 94:
      r = jni_term_to_jclass(ta1, &jc1, env) && jni_term_to_charP(ta2, &ch2) &&
          jni_term_to_charP(ta3, &ch3) &&
          jni_jfieldID_to_term((*env)->GetFieldID(env, jc1, ch2, ch3), tr);
      break;
    case 113:
      r = jni_term_to_jclass(ta1, &jc1, env) && jni_term_to_charP(ta2, &ch2) &&
          jni_term_to_charP(ta3, &ch3) &&
          jni_jmethodID_to_term((*env)->GetStaticMethodID(env, jc1, ch2, ch3),
                                tr);
      break;
    case 116:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jobject_to_term(
              (*env)->CallStaticObjectMethodA(env, jc1, jmi2, jvp3), tr, env);
      break;
    case 119:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jboolean_to_term(
              (*env)->CallStaticBooleanMethodA(env, jc1, jmi2, jvp3), tr);
      break;
    case 122:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jbyte_to_term((*env)->CallStaticByteMethodA(env, jc1, jmi2, jvp3),
                            tr);
      break;
    case 125:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jchar_to_term((*env)->CallStaticCharMethodA(env, jc1, jmi2, jvp3),
                            tr);
      break;
    case 128:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jshort_to_term(
              (*env)->CallStaticShortMethodA(env, jc1, jmi2, jvp3), tr);
      break;
    case 131:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jint_to_term((*env)->CallStaticIntMethodA(env, jc1, jmi2, jvp3),
                           tr);
      break;
    case 134:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jlong_to_term((*env)->CallStaticLongMethodA(env, jc1, jmi2, jvp3),
                            tr);
      break;
    case 137:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jfloat_to_term(
              (*env)->CallStaticFloatMethodA(env, jc1, jmi2, jvp3), tr);
      break;
    case 140:
      r = jni_term_to_jclass(ta1, &jc1, env) &&
          jni_term_to_jmethodID(ta2, &jmi2) &&
          jni_term_to_pointer(ta3, &jvp3) &&
          jni_jdouble_to_term(
              (*env)->CallStaticDoubleMethodA(env, jc1, jmi2, jvp3), tr);
      break;
    case 144:
      r = jni_term_to_jclass(ta1, &jc1, env) && jni_term_to_charP(ta2, &ch2) &&
          jni_term_to_charP(ta3, &ch3) &&
          jni_jfieldID_to_term((*env)->GetStaticFieldID(env, jc1, ch2, ch3),
                               tr);
      break;
    case 172:
      r = jni_term_to_non_neg_jsize(ta1, &js1) &&
          jni_term_to_jclass(ta2, &c2, env) &&
          jni_term_to_ref(ta3, &jo3, env) &&
          jni_jobject_to_term((*env)->NewObjectArray(env, js1, c2, jo3), tr,
                              env);
      break;
    default:
      return FALSE; /* oughta throw exception (design-time error :-) */
      break;
  }

  if (jvp3 != NULL)
  { free(jvp3);
  }

  return jni_check_exception(env) && r;
}

static foreign_t
jni_func_4_plc(term_t tn,  /* +integer: JNI function index */
               term_t ta1, /* +term: Arg1 */
               term_t ta2, /* +term: Arg2 */
               term_t ta3, /* +term: Arg3 */
               term_t ta4, /* +term: Arg4 */
               term_t tr   /* -term: Result */
               )
{ int     n; /* JNI function index */
  jvalue *jvp3 =
      NULL;   /* if this is given a buffer, it will be freed after the call */
  jboolean r; /* Prolog exit/fail outcome */
  JNIEnv * env;

  if (!jni_ensure_jvm() || !PL_get_integer(tn, &n))
  { return FALSE;
  }

  switch (n)
  { default:
      return FALSE; /* oughta throw exception (design-time error :-) */
      break;
  }

  if (jvp3 != NULL)
  { free(jvp3);
  }

  return jni_check_exception(env) && r;
}

/*=== JPL functions ======================================================= */

static int create_pool_engines();

static int
jpl_num_initial_default_args() /* used only once, by jpl_do_jpl_init() */
{ int i;

  for (i = 0; default_args[i] != NULL; i++)
  { }
  return i;
}

/* outcomes: */
/*      fail to find org.jpl7.*, org.jpl7.fli.* classes or to convert init args
 * to String[]: exception, FALSE */
/*      all OK: TRUE */
static bool
jpl_do_jpl_init(/* to be called once only, after PL init, before any JPL calls
                   */
                JNIEnv *env)
{ jclass  tc;  /* temporary class ref */
  jobject ta;  /* temporary array ref */
  char *  msg; /* error message for exceptions thrown here */
  int     i;   /* loop counter */
  jobject to;  /* temporary (String) object ref */

  if (jpl_status !=
      JPL_INIT_RAW) /* jpl init already attempted? (shouldn't happen) */
  { DEBUG(1, Sdprintf("[JPL: jpl_do_jpl_init() called AGAIN (skipping...)]\n"));
    return TRUE;
  }

  /* prerequisites for setting initial default args into String[] pvm_dia: */
  if ((tc = (*env)->FindClass(env, "java/lang/String")) == NULL ||
      (jString_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      ||
      (ta = (*env)->NewObjectArray(env, jpl_num_initial_default_args(),
                                   jString_c, NULL)) == NULL ||
      (pvm_dia = (*env)->NewGlobalRef(env, ta)) == NULL ||
      ((*env)->DeleteLocalRef(env, ta), FALSE))
  { msg = "jpl_do_jpl_init(): failed to find java.lang.String or create "
          "String[] pvm_dia";
    goto err;
  }

  /* copy the initial default args into String[] pvm_dia: */
  for (i = 0; default_args[i] != NULL; i++)
  { if ((to = (*env)->NewStringUTF(env, default_args[i])) == NULL)
    { msg = "jpl_do_jpl_init(): failed to convert an initial default arg to a "
            "String";
      goto err;
    }
    (*env)->SetObjectArrayElement(
        env, pvm_dia, i, to); /* any errors/exceptions to be handled here? */
  }

  if ((tc = (*env)->FindClass(env, "org/jpl7/JPLException")) == NULL ||
      (jJPLException_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      || (tc = (*env)->FindClass(env, "org/jpl7/fli/term_t")) == NULL ||
      (jTermT_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      || (tc = (*env)->FindClass(env, "org/jpl7/fli/atom_t")) == NULL ||
      (jAtomT_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      || (tc = (*env)->FindClass(env, "org/jpl7/fli/functor_t")) == NULL ||
      (jFunctorT_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      || (tc = (*env)->FindClass(env, "org/jpl7/fli/fid_t")) == NULL ||
      (jFidT_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      || (tc = (*env)->FindClass(env, "org/jpl7/fli/predicate_t")) == NULL ||
      (jPredicateT_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      || (tc = (*env)->FindClass(env, "org/jpl7/fli/qid_t")) == NULL ||
      (jQidT_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      || (tc = (*env)->FindClass(env, "org/jpl7/fli/module_t")) == NULL ||
      (jModuleT_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      || (tc = (*env)->FindClass(env, "org/jpl7/fli/engine_t")) == NULL ||
      (jEngineT_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      || (tc = (*env)->FindClass(env, "org/jpl7/fli/LongHolder")) == NULL ||
      (jLongHolder_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      || (tc = (*env)->FindClass(env, "org/jpl7/fli/PointerHolder")) == NULL ||
      (jPointerHolder_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      || (tc = (*env)->FindClass(env, "org/jpl7/fli/IntHolder")) == NULL ||
      (jIntHolder_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      || (tc = (*env)->FindClass(env, "org/jpl7/fli/Int64Holder")) == NULL ||
      (jInt64Holder_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      || (tc = (*env)->FindClass(env, "org/jpl7/fli/DoubleHolder")) == NULL ||
      (jDoubleHolder_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      || (tc = (*env)->FindClass(env, "org/jpl7/fli/StringHolder")) == NULL ||
      (jStringHolder_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      || (tc = (*env)->FindClass(env, "org/jpl7/fli/ObjectHolder")) == NULL ||
      (jObjectHolder_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      || (tc = (*env)->FindClass(env, "org/jpl7/fli/BooleanHolder")) == NULL ||
      (jBooleanHolder_c = (*env)->NewGlobalRef(env, tc)) == NULL ||
      ((*env)->DeleteLocalRef(env, tc), FALSE)

      ||
      (jLongHolderValue_f =
           (*env)->GetFieldID(env, jLongHolder_c, "value", "J")) == NULL

      ||
      (jPointerHolderValue_f =
           (*env)->GetFieldID(env, jPointerHolder_c, "value", "J")) == NULL

      ||
      (jIntHolderValue_f =
           (*env)->GetFieldID(env, jIntHolder_c, "value", "I")) == NULL

      ||
      (jInt64HolderValue_f =
           (*env)->GetFieldID(env, jInt64Holder_c, "value", "J")) == NULL

      ||
      (jDoubleHolderValue_f =
           (*env)->GetFieldID(env, jDoubleHolder_c, "value", "D")) == NULL

      ||
      (jStringHolderValue_f = (*env)->GetFieldID(env, jStringHolder_c, "value",
                                                 "Ljava/lang/String;")) == NULL

      ||
      (jObjectHolderValue_f = (*env)->GetFieldID(env, jObjectHolder_c, "value",
                                                 "Ljava/lang/Object;")) == NULL

      ||
      (jBooleanHolderValue_f =
           (*env)->GetFieldID(env, jBooleanHolder_c, "value", "Z")) == NULL)
  { msg = "jpl_do_jpl_init(): failed to find org.jpl7.* or org.jpl7.fli.* "
          "classes";
    goto err;
  }

  DEBUG(1, Sdprintf("[jpl_do_jpl_init() sets jpl_status = JPL_INIT_PVM_MAYBE, "
                    "returns TRUE]\n"));
  jpl_status = JPL_INIT_PVM_MAYBE;
  return TRUE;

err:
  jpl_status = JPL_INIT_JPL_FAILED;
  (*env)->ThrowNew(env, jJPLException_c, msg);
  return FALSE;
}

/* prerequisite: */
/*      called only from jpl_test_pvm_init() and jpl_do_pvm_init() */
/* outcomes: */
/*      error setting up post-PVM-init JPL state:  throws exception, sets status
 * = PVM_FAILED, returns FALSE */
/*      OK:  sets status = OK, returns TRUE */
static bool
jpl_post_pvm_init(JNIEnv *env, int argc, char **argv)
{ char *  msg;
  jobject ta;
  int     i;

    /* jpl_status = JPL_INIT_OK;
  return TRUE;*/
/* Prolog VM is already initialised (by us or by other party) */
  /* retire default init args and set up actual init args: */
  pvm_dia = NULL; /* probably oughta delete (global) ref to former args... */
  if ((ta = (*env)->NewObjectArray(env, argc, jString_c, NULL)) == NULL ||
      (pvm_aia = (*env)->NewGlobalRef(env, ta)) == NULL ||
      ((*env)->DeleteLocalRef(env, ta), FALSE))
  { msg = "jpl_post_pvm_init(): failed to copy actual init args";
    goto err;
  }
  for (i = 0; i < argc; i++)
  { jobject to;

    to = (*env)->NewStringUTF(env, argv[i]);
    if (to == NULL)
    { msg =
          "jpl_post_pvm_init(): failed to convert actual PL init arg to String";
      goto err;
    }
    (*env)->SetObjectArrayElement(env, pvm_aia, i, to);
  }
#ifndef __YAP_PROLOG__

  if (create_pool_engines() != 0)
  { msg = "jpl_post_pvm_init(): failed to create Prolog engine pool";
    goto err;
  }
#endif
  
  jpl_status = JPL_INIT_OK;
  return TRUE;

err:
  (*env)->ThrowNew(env, jJPLException_c, msg);
  jpl_status = JPL_INIT_PVM_FAILED;
  return FALSE;
}

/* prerequisite: jpl_status != JPL_INIT_RAW */
/* outcomes: */
/*      PVM is not (already) initialised -> FALSE */
/*      PVM is (already) initialised -> TRUE */
/*      error setting up post-PVM-init JPL state -> exception */
static bool
jpl_test_pvm_init(JNIEnv *env)
{ char * msg;
  int    argc;
  char **argv;
  /* jobject    ta; */
  /* int                i; */

  if (jpl_status == JPL_INIT_RAW)
  { msg = "jpl_test_pvm_init(): called while jpl_status == JPL_INIT_RAW";
    goto err;
  }

  if (jpl_status == JPL_INIT_JPL_FAILED || jpl_status == JPL_INIT_PVM_FAILED)
  { msg = "jpl_test_pvm_init(): initialisation has already failed";
    goto err;
  }

  if (jpl_status == JPL_INIT_OK)
  { return TRUE;
  }

  if (jpl_status == JPL_INIT_PVM_MAYBE)
  { /* we test this each time (if not already initialised) in case other foreign
     * code inits the PVM: */
    if (!PL_is_initialised(&argc, &argv)) /* PVM not ready? */
    { /* jpl_status remains = JPL_INIT_PVM_MAYBE */
      DEBUG(1, Sdprintf("[pl_test_pvm_init(): PL is not yet initialised: "
                        "returning FALSE]\n"));
      return FALSE; /* already-active Prolog VM not found (NB not an exceptional
                       condition) */
    } else
    { DEBUG(1, Sdprintf("[pl_test_pvm_init(): PL is already initialised: "
                        "proceeding to jpl_post_pvm_init()]\n"));
      return jpl_post_pvm_init(env, argc, argv); /* TRUE, FALSE or exception */
    }
  }

  msg = "jpl_test_pvm_init(): unknown jpl_status value";
  goto err;

err:
  (*env)->ThrowNew(env, jJPLException_c, msg);
  jpl_status = JPL_INIT_PVM_FAILED;
  return FALSE;
}

/* prerequisite: */
/*      jpl_status == JPL_INIT_PVM_MAYBE */
/* outcomes: */
/*      successful PVM initialisation and subsequent JPL state setup -> TRUE */
/*      any error -> exception */
static bool
jpl_do_pvm_init(JNIEnv *env)
{ char *  msg;
  int     argc;
  char ** argv;
  int     i;
  jstring arg;
  char *  cp;

  /* redundant prerequisites check: */
  if (jpl_status != JPL_INIT_PVM_MAYBE)
  { msg = "jpl_do_pvm_init(): called while jpl_status != JPL_INIT_PVM_MAYBE";
    goto err;
  }

  /* copy current default init args into suitable form for PL_initialise(): */
  if (pvm_dia == NULL)
  { msg = "jpl_do_pvm_init(): pvm_dia == NULL";
    goto err;
  }
  argc = (*env)->GetArrayLength(env, pvm_dia);
  if (argc <= 0)
  { msg = "jpl_do_pvm_init(): there are fewer than 1 default init args";
    goto err;
  }
  if ((argv = (char **)malloc((argc + 1) * sizeof(char *))) == NULL)
  { msg = "jpl_do_pvm_init(): malloc() failed for argv";
    goto err;
  }
  for (i = 0; i < argc; i++)
  { arg     = (jstring)(*env)->GetObjectArrayElement(env, pvm_dia, i);
    cp      = (char *)(*env)->GetStringUTFChars(env, arg, 0);
    argv[i] = (char *)malloc(strlen(cp) + 1);
    strcpy(argv[i], cp);
    DEBUG(1, Sdprintf("  argv[%d] = %s\n", i, argv[i]));
    (*env)->ReleaseStringUTFChars(env, arg, cp);
  }
  DEBUG(1, Sdprintf("   argv[%d] = NULL\n", argc));
  argv[argc] = NULL;
  if (!PL_initialise(argc, (char **)argv)) /* NB not (const char**) */
  { msg = "jpl_do_pvm_init(): PL_initialise() failed";
    goto err;
  }
  /* *don't* free argv (must exist for lifetime of Prolog VM) */

  return jpl_post_pvm_init(env, argc, argv); /* TRUE, FALSE or exception */

err:
  jpl_status = JPL_INIT_PVM_FAILED;
  (*env)->ThrowNew(env, jJPLException_c, msg);
  return FALSE;
}

static bool
jpl_ensure_jpl_init_1(JNIEnv *env)
{ bool r;

  pthread_mutex_lock(&jvm_init_mutex);
  r = jpl_do_jpl_init(env);
  pthread_mutex_unlock(&jvm_init_mutex);
  return r;
}

static bool
jpl_ensure_pvm_init_1(JNIEnv *env)
{ bool r;

  pthread_mutex_lock(&pvm_init_mutex);
  if (!jpl_ensure_jpl_init(env))
    return FALSE;
  r = jpl_test_pvm_init(env) || jpl_do_pvm_init(env);
  pthread_mutex_unlock(&pvm_init_mutex);
  return r;
}

/*=== initialisation-related native Java methods of org.jpl7.fli.Prolog ========
 */

/*
 * Class:         org_jpl7_fli_Prolog
 * Method:    get_default_init_args
 * Signature: ()[Ljava/lang/String;
 */
/* if not yet init then return default init args as String[] */
/* if already init then return NULL */
/* if already failed to init then throw an exception */
JNIEXPORT jobject JNICALL
Java_org_jpl7_fli_Prolog_get_1default_1init_1args(JNIEnv *env, jclass jProlog)
{ char *msg;

  if (!jpl_ensure_jpl_init(
          env)) /* lazily do "local" initialisations iff necessary */
    return FALSE;

  if (jpl_status == JPL_INIT_JPL_FAILED || jpl_status == JPL_INIT_PVM_FAILED)
  { msg = "org.jpl7.fli.Prolog.set_default_init_args(): initialisation has "
          "already failed";
    goto err;
  }

  return (jpl_test_pvm_init(env) /* if Prolog VM is initialised */
              ? NULL    /* then default init args are no longer defined */
              : pvm_dia /* else here they are */
          );
err:
  (*env)->ThrowNew(env, jJPLException_c, msg);
  return FALSE;
}

/*
 * Class:         org_jpl7_fli_Prolog
 * Method:    set_default_init_args
 * Signature: ([Ljava/lang/String;)Z
 */
/* if the given jargs are null then throw an exception */
/* if already failed to init then throw an exception */
/* if not yet init then set default init args from jargs and return TRUE */
/* if already init then return FALSE */
JNIEXPORT jboolean JNICALL
                   Java_org_jpl7_fli_Prolog_set_1default_1init_1args(
    JNIEnv *env, jclass jProlog,
    jobject jargs /* oughta be proper array, perhaps zero-length */
    )
{ char *msg;

  if (!jpl_ensure_jpl_init(
          env)) /* lazily do "local" initialisations iff necessary */
    return FALSE;

  if (jargs == NULL) /* improper call */
  { msg = "org.jpl7.fli.Prolog.set_default_init_args() called with NULL arg";
    goto err;
  }

  if (jpl_status == JPL_INIT_JPL_FAILED || jpl_status == JPL_INIT_PVM_FAILED)
  { msg = "org.jpl7.fli.Prolog.set_default_init_args(): initialisation has "
          "already failed";
    goto err;
  }

  if (jpl_test_pvm_init(env)) /* if Prolog VM is initialised */
  { return FALSE; /* unable to set default init args (too late: PVM is already
                     initialised) */
  } else
  { pvm_dia =
        NULL; /* probably oughta delete (global) (?) ref of former args... */
    pvm_dia = (*env)->NewGlobalRef(env, jargs);
    return TRUE; /* OK: default init args set to those provided */
  }

err:
  (*env)->ThrowNew(env, jJPLException_c, msg);
  return FALSE;
}

/*
 * Class:         org_jpl7_fli_Prolog
 * Method:    get_actual_init_args
 * Signature: ()[Ljava/lang/String;
 */
/* if not yet init then return null */
/* if already init then return actual init args as String[] */
/* if already failed to init then throw an exception */
JNIEXPORT jobject JNICALL
Java_org_jpl7_fli_Prolog_get_1actual_1init_1args(JNIEnv *env, jclass jProlog)
{ char *msg;

  if (!jpl_ensure_jpl_init(
          env)) /* lazily do "local" initialisations iff necessary */
    return NULL;

  if (jpl_status == JPL_INIT_JPL_FAILED || jpl_status == JPL_INIT_PVM_FAILED)
  { msg = "org.jpl7.fli.Prolog.get_actual_init_args(): initialisation has "
          "already failed";
    goto err;
  }

  return (
      jpl_test_pvm_init(
          env) /* check PL_initialise() and update local state as appropriate */
          ? pvm_aia /* here they are */
          : NULL    /* PVM not (yet) initialised */
      );

err:
  (*env)->ThrowNew(env, jJPLException_c, msg);
  return NULL;
}

/*
 * Class:         org_jpl7_fli_Prolog
 * Method:    initialise
 * Signature: ()Z
 */
/* if already init then return FALSE */
/* if already failed to init then throw an exception */
/* else attempt to init and if success then return TRUE else throw an exception
 */
JNIEXPORT jboolean JNICALL
Java_org_jpl7_fli_Prolog_initialise(JNIEnv *env, jclass jProlog)
{ char *msg;

  if (!jpl_ensure_jpl_init(
          env)) /* lazily do "local" initialisations iff necessary */
    return FALSE;

  if (jpl_status == JPL_INIT_JPL_FAILED || jpl_status == JPL_INIT_PVM_FAILED)
  { msg = "org.jpl7.fli.Prolog.initialise(): initialisation has already failed";
    goto err;
  }

  if (jpl_test_pvm_init(env))
  { return FALSE; /* PVM is already initialised */
  } else
  { jpl_do_pvm_init(env);
    return jpl_test_pvm_init(env);
  }

err:
  (*env)->ThrowNew(env, jJPLException_c, msg);
  return FALSE;
}

/*
 * Class:         org_jpl7_fli_Prolog
 * Method:    halt
 * Signature: (I)V
 */
JNIEXPORT void JNICALL
Java_org_jpl7_fli_Prolog_halt(JNIEnv *env, jclass jProlog, jint jstatus)
{ (void)jpl_ensure_pvm_init(env);
  PL_halt((int)jstatus);
}

/*=== JPL utility functions =============================================== */

/*-----------------------------------------------------------------------
 * getLongValue
 *
 * Retrieves the value in a org.jpl7.fli.LongHolder (or subclass) instance
 *
 * @param   env           Java environment
 * @param   jlong_holder  the LongHolder class instance, or null
 * @param   lv            address to write the retrieved (long) value
 * @return                success? (the LongHolder was not null)
 *---------------------------------------------------------------------*/
static bool
getLongValue(JNIEnv *env, jobject jlong_holder, jlong *lv)
{ if (jlong_holder == NULL)
  { *lv = 0L;
    return FALSE;
  } else /* Java compilation ensures it's a org.jpl7.fli.LongHolder instance */
  { *lv = (*env)->GetLongField(env, jlong_holder, jLongHolderValue_f);
    return TRUE;
  }
}

static bool
getUIntPtrValue(JNIEnv *env, jobject jlong_holder, uintptr_t *iv)
{ jlong lv; /* Java long is 64 bits */

  *iv = 0;
  if (getLongValue(env, jlong_holder, &lv))
  {
#if SIZEOF_VOIDP == 4
    if (lv >= 0xffffffffLL)
      return FALSE; /* What to do? */
#endif
    *iv = (uintptr_t)lv;
    return TRUE;
  } else
  { return FALSE;
  }
}

static bool
getTermValue(JNIEnv *env, jobject jlong_holder, term_t *t)
{ return getUIntPtrValue(env, jlong_holder, t);
}

static bool
getAtomValue(JNIEnv *env, jobject jlong_holder, atom_t *a)
{ return getUIntPtrValue(env, jlong_holder, a);
}

static bool
getFunctorValue(JNIEnv *env, jobject jlong_holder, functor_t *f)
{ return getUIntPtrValue(env, jlong_holder, f);
}

static bool
getQIDValue(JNIEnv *env, jobject jlong_holder, qid_t *q)
{ return getUIntPtrValue(env, jlong_holder, q);
}

static bool
getFIDValue(JNIEnv *env, jobject jlong_holder, fid_t *f)
{ return getUIntPtrValue(env, jlong_holder, f);
}


/*-----------------------------------------------------------------------
 * getPointerValue
 *
 * Retrieves the value in a org.jpl7.fli.PointerHolder instance
 *
 * @param   env              Java environment
 * @param   jpointer_holder  the PointerHolder class instance, or null
 * @param   pv               address to write the retrieved (pointer) value
 * @return                   success? (the PointerHolder was not null)
 *---------------------------------------------------------------------*/
static bool
getPointerValue(/* sets pv to jpointer_holder's .value_ (and succeeds), else
                   sets it to NULL (and fails) */
                JNIEnv *env,
                jobject jpointer_holder, pointer *pv)
{ if (jpointer_holder == NULL)
  { *pv = (pointer)NULL;
    return FALSE;
  } else /* Java compilation ensures it's a org.jpl7.fli.PointerHolder instance
            */
  { *pv = (pointer)(*env)->GetLongField(env, jpointer_holder,
                                        jPointerHolderValue_f);
    return TRUE;
  }
}

/*-----------------------------------------------------------------------
 * setPointerValue
 *
 * Sets the value in a org.jpl7.fli.Pointer class instance (unless it's null)
 * to the supplied value (maybe 0L)
 *
 * @param   env              Java environment
 * @param   jpointer_holder  the PointerHolder class instance, or null
 * @param   pv               the new (pointer) value
 *---------------------------------------------------------------------*/
static bool
setPointerValue(JNIEnv *env, jobject jpointer_holder, pointer pv)
{ return jpointer_holder != NULL &&
         ((*env)->SetLongField(env, jpointer_holder, jPointerHolderValue_f,
                               (long)pv),
          TRUE);
}

/*-----------------------------------------------------------------------
 * setIntValue
 *
 * Sets the value in a Java IntHolder class instance (unless it's null)
 * to the supplied value
 *
 * @param   env          Java environment
 * @param   jint_holder  the IntHolder class instance, or null
 * @param   iv           the new (int) value
 *---------------------------------------------------------------------*/
static bool
setIntValue(JNIEnv *env, jobject jint_holder, jint iv)
{ return jint_holder != NULL &&
         ((*env)->SetIntField(env, jint_holder, jIntHolderValue_f, iv), TRUE);
}

/*-----------------------------------------------------------------------
 * setLongValue
 *
 * Sets the value in a Java LongHolder class instance (unless it's null)
 * to the supplied Java long value
 *
 * @param   env           Java environment
 * @param   jlong_holder  the LongHolder class instance, or null
 * @param   lv            the new (Java long) value
 *---------------------------------------------------------------------*/
static bool
setLongValue(JNIEnv *env, jobject jlong_holder, jlong lv)
{ return jlong_holder != NULL &&
         ((*env)->SetLongField(env, jlong_holder, jLongHolderValue_f, lv),
          TRUE);
}

static bool
setUIntPtrValue(JNIEnv *env, jobject jlong_holder, uintptr_t iv)
{ jlong lv;

#if SIZEOF_VOIDP == 4
  uint64_t i64 = iv; /* unsigned 32->64 */
  lv           = (jlong)i64;
#else
  lv       = iv;
#endif

  return setLongValue(env, jlong_holder, lv);
}

/*-----------------------------------------------------------------------
 * setDoubleValue
 *
 * Sets the value in a Java DoubleHolder class instance (unless it's null)
 * to the supplied value
 *
 * @param   env             Java environment
 * @param   jdouble_holder  the DoubleHolder class instance, or null
 * @param   dv              the new (double) value
 *---------------------------------------------------------------------*/
static bool
setDoubleValue(JNIEnv *env, jobject jdouble_holder, jdouble dv)
{ return jdouble_holder != NULL &&
         ((*env)->SetDoubleField(env, jdouble_holder, jDoubleHolderValue_f, dv),
          TRUE);
}

/*-----------------------------------------------------------------------
 * setStringValue
 *
 * Sets the value in a Java StringHolder class instance (unless it's null)
 * to the supplied value (maybe null)
 *
 * @param   env             Java environment
 * @param   jstring_holder  the StringHolder class instance, or null
 * @param   sv              the new (jstring) value
 *---------------------------------------------------------------------*/
static bool
setStringValue(JNIEnv *env, jobject jstring_holder, jstring sv)
{ return jstring_holder != NULL &&
         ((*env)->SetObjectField(env, jstring_holder, jStringHolderValue_f, sv),
          TRUE);
}

/*-----------------------------------------------------------------------
 * setObjectValue
 *
 * Sets the value in a Java ObjectHolder class instance (unless it's null)
 * to the supplied value (maybe null)
 *
 * @param   env             Java environment
 * @param   jobject_holder  the ObjectHolder class instance, or null
 * @param   ref             the new (jobject) value
 *---------------------------------------------------------------------*/
static bool
setObjectValue(JNIEnv *env, jobject jobject_holder, jobject ref)
{ return jobject_holder != NULL &&
         ((*env)->SetObjectField(env, jobject_holder, jObjectHolderValue_f,
                                 ref),
          TRUE);
}

/*=== Java-wrapped SWI-Prolog FLI functions =============================== */

static int current_pool_engine_handle(PL_engine_t *e);
static int current_pool_engine();

/*
 * Class:         org_jpl7_fli_Prolog
 * Method:        action_abort
 * Signature: ()I
 */
JNIEXPORT int JNICALL
Java_org_jpl7_fli_Prolog_action_1abort(JNIEnv *env, jclass jProlog)
{ if (jpl_ensure_pvm_init(env))
  { return PL_action(PL_ACTION_ABORT);
  } else
  { return -2; /* oughta throw exception? */
  }
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    atom_chars
 * Signature: (Lorg/jpl7/fli/atom_t;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_org_jpl7_fli_Prolog_atom_1chars(JNIEnv *env, jclass jProlog,
                                     jobject jatom)
{ atom_t  atom;
  jstring lref;

  if ( jpl_ensure_pvm_init(env) &&
       getAtomValue(env, jatom, &atom) &&
       jni_atom_to_String(env, atom, &lref) )
    return lref;

  return NULL;
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    attach_engine
 * Signature: (Lorg/jpl7/fli/engine_t;)I
 */
JNIEXPORT int JNICALL
Java_org_jpl7_fli_Prolog_attach_1engine(JNIEnv *env, jclass jProlog,
                                        jobject jengine)
{ PL_engine_t engine;
  int         rc;

  if ( !jpl_ensure_pvm_init(env) )
  { return -2; /* libpl could not be initialised (oughta throw exception) */
  }

  rc = current_pool_engine_handle(&engine);
  DEBUG(0,
        Sdprintf(
            "attach_engine(): current_engine=%p, thread_self=%d, pool_id=%d\n",
            engine, PL_thread_self(), rc));

  if ( !getPointerValue(env, jengine, (pointer *)&engine) )
  { return -3; /* null engine holder */
  }

  DEBUG(0, Sdprintf("attach_engine(): new_engine=%p\n", engine));

  if ((rc = PL_set_engine(engine, NULL)) == PL_ENGINE_SET)
  { return 0; /* OK */
  } else
  { return -1; /* bad engine status: oughta throw exception */
  }
}

/*
 * Class:         org_jpl7_fli_Prolog
 * Method:        close_query
 * Signature: (Lorg/jpl7/fli/qid_t;)V
 */
JNIEXPORT void JNICALL
Java_org_jpl7_fli_Prolog_close_1query(JNIEnv *env, jclass jProlog, jobject jqid)
{ qid_t qid;

  DEBUG(1, Sdprintf(">close_query(env=%p,jProlog=%p,jquid=%p)...\n", env,
                    jProlog, jqid));
  if ( jpl_ensure_pvm_init(env) &&
       getQIDValue(env, jqid, &qid) )
  { PL_close_query(qid);
    DEBUG(1, Sdprintf("  ok: PL_close_query(%lu)\n", (long)qid));
  }
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    compare
 * Signature: (Lorg/jpl7/fli/term_t;Lorg/jpl7/fli/term_t;)I
 */

JNIEXPORT jint JNICALL /* returns -1, 0 or 1 (or -2 for error) */
Java_org_jpl7_fli_Prolog_compare(JNIEnv *env, jclass jProlog,
                                 jobject jterm1, jobject jterm2)
{ term_t term1;
  term_t term2;

  DEBUG(1, Sdprintf(">compare(term1=%p,term2=%p)\n", jterm1, jterm2));
  if ( jpl_ensure_pvm_init(env) &&
       getTermValue(env, jterm1, &term1) &&
       getTermValue(env, jterm2, &term2) )
  { DEBUG(1, Sdprintf("> PL_compare( %u, %u)", term1, term2));
    return PL_compare(term1, term2); /* returns -1, 0 or 1 */
  } else
  { return -2; /* oughta throw an exception... */
  }
}

/*
 * Class:         org_jpl7_fli_Prolog
 * Method:        cons_functor_v
 * Signature:
 * (Lorg/jpl7/fli/term_t;Lorg/jpl7/fli/functor_t;Lorg/jpl7/fli/term_t;)V
 */
JNIEXPORT jboolean JNICALL
Java_org_jpl7_fli_Prolog_cons_1functor_1v(JNIEnv *env, jclass jProlog,
                                          jobject jterm, jobject jfunctor,
                                          jobject jterm0)
{ term_t    term;
  functor_t functor;
  term_t    term0;

  if ( jpl_ensure_pvm_init(env) &&
       getTermValue(env, jterm, &term) &&
       getFunctorValue(env, jfunctor, &functor) &&
       getTermValue(env, jterm0, &term0) )
    return PL_cons_functor_v(term, functor, term0);

  return TRUE;                                  /* FIXME: Why not FALSE? */
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    copy_term_ref
 * Signature: (Lorg/jpl7/fli/term_t;)Lorg/jpl7/fli/term_t;
 */
JNIEXPORT jobject JNICALL
Java_org_jpl7_fli_Prolog_copy_1term_1ref(JNIEnv *env, jclass jProlog,
                                         jobject jfrom)
{ jobject rval;
  term_t  term;
  term_t  term2;

  if ( jpl_ensure_pvm_init(env) &&
       getTermValue(env, jfrom, &term) &&
       (rval = (*env)->AllocObject(env, jTermT_c)) &&
       (term2 = PL_copy_term_ref(term)) &&
       setUIntPtrValue(env, rval, term2) )
    return rval;

  return NULL;                          /* oughta warn of failure? */
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    current_query
 * Signature: ()Lorg/jpl7/fli/qid_t;
 */
JNIEXPORT jobject JNICALL
Java_org_jpl7_fli_Prolog_current_1query(JNIEnv *env, jclass jProlog)
{ jobject rval;
  qid_t   qid;

  if ( jpl_ensure_pvm_init(env) &&
       (qid = PL_current_query()) &&
       (rval = (*env)->AllocObject(env, jQidT_c)) &&
       setUIntPtrValue(env, rval, qid) )
    return rval;

  return NULL;                                  /* oughta throw exception */
}


/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    current_engine
 * Signature: ()Lorg/jpl7/fli/engine_t;
 */
JNIEXPORT jobject JNICALL
Java_org_jpl7_fli_Prolog_current_1engine(JNIEnv *env, jclass jProlog)
{ PL_engine_t engine;
  jobject     rval;

  if ( jpl_ensure_pvm_init(env) &&
       PL_thread_self() != -1 &&
       (current_pool_engine_handle(&engine), TRUE) &&
       (rval = (*env)->AllocObject(env, jEngineT_c)) &&
       setPointerValue(env, rval, (pointer)engine) )
    return rval;

  return NULL;                                  /* oughta throw exception */
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    current_engine_is_pool
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL
Java_org_jpl7_fli_Prolog_current_1engine_1is_1pool(JNIEnv *env, jclass jProlog)
{ if ( jpl_ensure_pvm_init(env) )
  { return current_pool_engine() >= 0;
  } else
  { return FALSE;                               /* oughta throw exception */
  }
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    exception
 * Signature: (Lorg/jpl7/fli/qid_t;)Lorg/jpl7/fli/term_t;
 */
JNIEXPORT jobject JNICALL
Java_org_jpl7_fli_Prolog_exception(JNIEnv *env, jclass jProlog, jobject jqid)
{ qid_t   qid;
  term_t  term;
  jobject term_t; /* return value */

  DEBUG(1, Sdprintf(">exception(jqid=%p)\n", jqid));
  return (
      jpl_ensure_pvm_init(env) &&
              (DEBUG(1, Sdprintf("      ok: jpl_ensure_pvm_init(env)\n")), TRUE)
              /* &&     jqid != NULL    // redundant */
              && (DEBUG(1, Sdprintf("   ok: jqid != NULL\n")), TRUE) &&
              getQIDValue(env, jqid, &qid)
              && (DEBUG(1, Sdprintf("   ok: getQIDValue(env,jqid,&qid)\n")),
                  TRUE) &&
              ((term = PL_exception(qid)),
               TRUE) /* we'll build a term_t object regardless */
              &&
              (DEBUG(1, Sdprintf("  ok: ( (term=PL_exception(qid)), TRUE)\n")),
               TRUE) &&
              (term_t = (*env)->AllocObject(env, jTermT_c)) != NULL &&
              (DEBUG(1, Sdprintf("      ok: "
                                 "(term_t=(*env)->AllocObject(env,jTermT_c)) "
                                 "!= NULL\n")),
               TRUE) &&
              setUIntPtrValue(env, term_t, term) &&
              (DEBUG(1,
                     Sdprintf(" ok: setUIntPtrValue(env,term_t,term)\n")),
               TRUE)
          ? (DEBUG(1, Sdprintf("  =%p\n", term_t)), term_t)
          : NULL /* oughta diagnose failure? */
      );
}

/*
 * Class:         org_jpl7_fli_Prolog
 * Method:        get_arg
 * Signature: (ILorg/jpl7/fli/term_t;Lorg/jpl7/fli/term_t;)Z
 */
JNIEXPORT jboolean JNICALL
Java_org_jpl7_fli_Prolog_get_1arg(JNIEnv *env, jclass jProlog, jint jindex,
                                  jobject jterm, jobject jarg)
{ term_t term;
  term_t arg;

  return ( jpl_ensure_pvm_init(env) &&
           jarg != NULL &&
           getTermValue(env, jterm, &term) &&
           (arg = PL_new_term_ref()) &&
           PL_get_arg(jindex, term, arg) &&
           setUIntPtrValue(env, jarg, arg) );
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    get_atom_chars
 * Signature: (Lorg/jpl7/fli/term_t;Lorg/jpl7/fli/StringHolder;)Z
 */
JNIEXPORT jboolean JNICALL
Java_org_jpl7_fli_Prolog_get_1atom_1chars(JNIEnv *env, jclass jProlog,
                                          jobject jterm, jobject jstring_holder)
{ term_t  term;
  atom_t  a;
  jstring string;

  return ( jpl_ensure_pvm_init(env) &&
           jstring_holder != NULL &&
           getTermValue(env, jterm, &term) &&
           PL_get_atom(term, &a) &&
           jni_atom_to_String(env, a, &string) &&
           setStringValue(env, jstring_holder, string) );
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    get_jref_object
 * Signature: (Lorg/jpl7/fli/term_t;Lorg/jpl7/fli/ObjectHolder;)Z
 */
JNIEXPORT jboolean JNICALL
Java_org_jpl7_fli_Prolog_get_1jref_1object(JNIEnv *env, jclass jProlog,
                                           jobject jterm,
                                           jobject jobject_holder)
{ term_t       term;
  atom_t       a;
  jref_handle *ref;
  PL_blob_t *  type;

  return ( jpl_ensure_pvm_init(env) &&
           jobject_holder != NULL &&
           getTermValue(env, jterm, &term) &&
           PL_get_atom(term, &a) &&
           (ref = PL_blob_data(a, NULL, &type)) &&
           type == &jref_blob &&
           setObjectValue(env, jobject_holder, (void *)ref->iref) );
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    get_c_lib_version
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jobject JNICALL
Java_org_jpl7_fli_Prolog_get_1c_1lib_1version(JNIEnv *env, jclass jProlog)
{ return (*env)->NewStringUTF(env, JPL_C_LIB_VERSION);
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    get_float
 * Signature: (Lorg/jpl7/fli/term_t;Lorg/jpl7/fli/DoubleHolder;)Z
 */
JNIEXPORT jboolean JNICALL
Java_org_jpl7_fli_Prolog_get_1float(JNIEnv *env, jclass jProlog, jobject jterm,
                                    jobject jdouble_holder)
{ term_t term;
  double d;

  return ( jpl_ensure_pvm_init(env) &&
           jdouble_holder != NULL &&
           getTermValue(env, jterm, &term) &&
           PL_get_float(term, &d) &&
           setDoubleValue(env, jdouble_holder, d) );
}

/*
 * Class:         org_jpl7_fli_Prolog
 * Method:        get_integer
 * Signature: (Lorg/jpl7/fli/term_t;Lorg/jpl7/fli/Int64Holder;)Z
 */
JNIEXPORT jboolean JNICALL
Java_org_jpl7_fli_Prolog_get_1integer(JNIEnv *env, jclass jProlog,
                                      jobject jterm, jobject jint64_holder)
{ term_t  term;
  int64_t i64;

  return ( jpl_ensure_pvm_init(env) &&
           jint64_holder != NULL &&
           getTermValue(env, jterm, &term) &&
           PL_get_int64(term, &i64) &&
           setLongValue(env, jint64_holder, i64) );
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    get_integer_big
 * Signature: (Lorg/jpl7/fli/term_t;Lorg/jpl7/fli/StringHolder;)Z
 */
JNIEXPORT jboolean JNICALL
Java_org_jpl7_fli_Prolog_get_1integer_1big(
    JNIEnv *env, jclass jProlog, jobject jterm,
    jobject jbigint_holder /* we trust this is a StringHolder */
    )
{ term_t  term;
  char *  bigint;
  jstring jbigint;

  return ( jpl_ensure_pvm_init(env) &&
           jbigint_holder != NULL &&
           getTermValue(env, jterm, &term) &&
           PL_get_chars(term, &bigint, CVT_INTEGER) &&
           (jbigint = (*env)->NewStringUTF(env, bigint)) &&
           setStringValue(env, jbigint_holder, jbigint) );
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    get_name_arity
 * Signature: (Lorg/jpl7/fli/term_t;Lorg/jpl7/fli/StringHolder;Lorg/jpl7/fli/IntHolder;)Z
 */
JNIEXPORT jboolean JNICALL
Java_org_jpl7_fli_Prolog_get_1name_1arity(
    JNIEnv *env, jclass jProlog, jobject jterm,
    jobject jname_holder, jobject jarity_holder)
{ term_t  term;
  atom_t  atom;
  jstring jname;
  size_t  arity;

  return ( jpl_ensure_pvm_init(env) &&
           jname_holder != NULL &&
           jarity_holder != NULL &&
           getTermValue(env, jterm, &term) &&
           PL_get_name_arity(term, &atom, &arity) &&
           jni_atom_to_String(env, atom, &jname) &&
           setStringValue(env, jname_holder, jname) &&
           setIntValue(env, jarity_holder, arity) );
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    get_string_chars
 * Signature: (Lorg/jpl7/fli/term_t;Lorg/jpl7/fli/StringHolder;)Z
 */
JNIEXPORT jboolean JNICALL
Java_org_jpl7_fli_Prolog_get_1string_1chars(JNIEnv *env, jclass jProlog,
                                            jobject jterm,
                                            jobject jstring_holder)
{ term_t  term;
  jstring string;

  return ( jpl_ensure_pvm_init(env) &&
           jstring_holder != NULL &&
           getTermValue(env, jterm, &term) &&
           jni_string_to_String(env, term, &string) &&
           setStringValue(env, jstring_holder, string) );
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    new_atom
 * Signature: (Ljava/lang/String;)Lorg/jpl7/fli/atom_t;
 */
JNIEXPORT jobject JNICALL
Java_org_jpl7_fli_Prolog_new_1atom(JNIEnv *env, jclass jProlog, jstring jname)
{ atom_t  atom;
  jobject rval;

  if ( jpl_ensure_pvm_init(env) &&
       jni_String_to_atom(env, jname, &atom) &&
       (rval = (*env)->AllocObject(env, jAtomT_c)) &&
       setUIntPtrValue(env, rval, atom) )
    return rval;

  return NULL;                                  /* oughta warn of failure? */
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    new_functor
 * Signature: (Lorg/jpl7/fli/atom_t;I)Lorg/jpl7/fli/functor_t;
 */
JNIEXPORT jobject JNICALL
Java_org_jpl7_fli_Prolog_new_1functor(JNIEnv *env, jclass jProlog,
                                      jobject jatom, /* read-only */
                                      jint    jarity)
{ atom_t    atom;
  functor_t functor;
  jobject   rval;

  if ( jpl_ensure_pvm_init(env) && jarity >= 0 &&
       getAtomValue(env, jatom, &atom) &&
       (rval = (*env)->AllocObject(env, jFunctorT_c)) &&
       (functor = PL_new_functor(atom, (int)jarity)) &&
       setUIntPtrValue(env, rval, functor) )
    return rval;

  return NULL;                                  /* oughta warn of failure? */
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    new_module
 * Signature: (Lorg/jpl7/fli/atom_t;)Lorg/jpl7/fli/module_t;
 */
JNIEXPORT jobject JNICALL
Java_org_jpl7_fli_Prolog_new_1module(JNIEnv *env, jclass jProlog, jobject jatom)
{ atom_t   atom;
  module_t module;
  jobject  rval;

  if ( jpl_ensure_pvm_init(env) &&
       getAtomValue(env, jatom, &atom) &&
       (module = PL_new_module(atom)) &&
       (rval = (*env)->AllocObject(env, jModuleT_c)) &&
       setPointerValue(env, rval, (pointer)module) )
    return rval;

  return NULL;                                  /* oughta warn of failure? */
}

/*
 * Class:         org_jpl7_fli_Prolog
 * Method:        new_term_ref
 * Signature: ()Lorg/jpl7/fli/term_t;
 */
JNIEXPORT jobject JNICALL
Java_org_jpl7_fli_Prolog_new_1term_1ref(JNIEnv *env, jclass jProlog)
{ jobject rval;
  term_t t;

  if ( jpl_ensure_pvm_init(env) &&
       (rval = (*env)->AllocObject(env, jTermT_c)) &&
       (t = PL_new_term_ref()) &&
       setUIntPtrValue(env, rval, t) )
    return rval;

  return NULL;
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    new_term_refs
 * Signature: (I)Lorg/jpl7/fli/term_t;
 */
JNIEXPORT jobject JNICALL
Java_org_jpl7_fli_Prolog_new_1term_1refs(JNIEnv *env, jclass jProlog, jint jn)
{ jobject rval;
  term_t  trefs;

  DEBUG(1, Sdprintf(">new_term_refs(env=%p,jProlog=%p,jn=%p)...\n", env,
                    jProlog, jn));

  if ( jpl_ensure_pvm_init(env) &&
       jn >= 0 &&
       (rval = (*env)->AllocObject(env, jTermT_c)) &&
       (trefs = PL_new_term_refs((int)jn), TRUE) &&
       setUIntPtrValue(env, rval, trefs) )
  { DEBUG(1, Sdprintf("  ok: stashed trefs=%ld into new term_t object\n",
                      (long)trefs));
    return rval;
  }

  return NULL;
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    next_solution
 * Signature: (Lorg/jpl7/fli/qid_t;)Z
 */
JNIEXPORT jboolean JNICALL
Java_org_jpl7_fli_Prolog_next_1solution(JNIEnv *env, jclass jProlog,
                                        jobject jqid /* read */
                                        )
{ qid_t qid = 0;                                /* make compiler happy */

  DEBUG(1, Sdprintf(">next_solution(env=%p,jProlog=%p,jqid=%p)...\n", env,
                    jProlog, jqid));

  if ( jpl_ensure_pvm_init(env) &&
       getQIDValue(env, jqid, &qid) &&
       PL_next_solution(qid) )
  { DEBUG(1, Sdprintf(" ok: PL_next_solution(qid=%lu)=TRUE\n", (long)qid));
    return TRUE;
  }

  DEBUG(1, Sdprintf("   ok: PL_next_solution(qid=%lu)=FALSE\n", (long)qid));
  return FALSE;
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    open_query
 * Signature: (Lorg/jpl7/fli/module_t;ILorg/jpl7/fli/predicate_t;Lorg/jpl7/fli/term_t;)Lorg/jpl7/fli/qid_t;
 */

JNIEXPORT jobject JNICALL
Java_org_jpl7_fli_Prolog_open_1query(JNIEnv *env, jclass jProlog,
                                     jobject jmodule,    /* read */
                                     jint    jflags,     /* read */
                                     jobject jpredicate, /* read */
                                     jobject jterm0      /* read */
                                     )
{ module_t    module;
  predicate_t predicate;
  term_t      term0;
  qid_t       qid;
  jobject     jqid; /* for returned new QidT object */

  DEBUG(1, Sdprintf(">open_query(env=%p,jProlog=%p,jmodule=%p,jflags=%p,"
                    "jpredicate=%p,jterm0=%p)...\n",
                    env, jProlog, jmodule, jflags, jpredicate, jterm0));
  return (
      jpl_ensure_pvm_init(env) &&
              (getPointerValue(env, jmodule, (pointer *)&module),
               TRUE) /* NULL module is OK below... */
              && (DEBUG(1, Sdprintf("  ok: "
                                    "getPointerValue(env,jmodule=%p,&(pointer)"
                                    "module=%p)\n",
                                    jmodule, module)),
                  TRUE) &&
              getPointerValue(
                  env, jpredicate,
                  (pointer *)&predicate) /* checks that jpredicate != NULL */
              && (DEBUG(1, Sdprintf("  ok: "
                                    "getPointerValue(env,jpredicate=%p,&("
                                    "pointer)predicate=%p)\n",
                                    jpredicate, predicate)),
                  TRUE) &&
              getTermValue(env, jterm0, &term0)
              && ((qid = PL_open_query(module, jflags, predicate, term0)),
                  TRUE) /* NULL module is OK (?) [ISSUE] */
              && (DEBUG(1, Sdprintf("  ok: "
                                    "PL_open_query(module=%p,jflags=%u,"
                                    "predicate=%p,term0=%d)=%p\n",
                                    module, jflags, predicate, term0, qid)),
                  TRUE) &&
              (jqid = (*env)->AllocObject(env, jQidT_c)) != NULL &&
              (DEBUG(1, Sdprintf("  ok: AllocObject(env,jQidT_c)=%p\n", jqid)),
               TRUE) &&
              setUIntPtrValue(env, jqid, qid) &&
              (DEBUG(1,
                     Sdprintf("  ok: setUIntPtrValue(env,%p,%p)\n", jqid, qid)),
               TRUE) &&
              (DEBUG(1, Sdprintf("[open_query module = %s]\n",
                                 (module == NULL ? "(null)"
                                                 : PL_atom_chars(PL_module_name(
                                                       module))))),
               TRUE)
          ? (DEBUG(1, Sdprintf("  =%p\n", jqid)), jqid)
          : NULL /* oughta diagnose failure? raise JPL exception? */
      );
}

/*
 * Class:         org_jpl7_fli_Prolog
 * Method:        predicate
 * Signature: (Ljava/lang/String;ILjava/lang/String;)Lorg/jpl7/fli/predicate_t;
 */
JNIEXPORT jobject JNICALL
Java_org_jpl7_fli_Prolog_predicate(JNIEnv *env, jclass jProlog,
                                   jstring jname,  /* ought not be null */
                                   jint    jarity, /* oughta be >= 0 */
                                   jstring jmodule /* may be null */
                                   )
{ atom_t      pname; /* the predicate's name, as an atom */
  atom_t      mname; /* the predicate's module's name, as an atom */
  functor_t   func;  /* the predicate's functor */
  module_t    mod;   /* the predicate's module */
  predicate_t predicate;
  jobject     rval;

  DEBUG(1,
        Sdprintf(
            ">predicate(env=%p,jProlog=%p,jname=%p,jarity=%p,jmodule=%p)...\n",
            env, jProlog, jname, jarity, jmodule));
  return (jpl_ensure_pvm_init(env) &&
                  jni_String_to_atom(env, jname,
                                     &pname) /* checks that jname isn't NULL */
                  && jarity >= 0 && (func = PL_new_functor(pname, jarity),
                                     TRUE) /* "cannot fail" */
                  && (jmodule != NULL
                          ? jni_String_to_atom(
                                env, jmodule,
                                &mname) /* checks that jmodule isn't NULL */
                          : (mname = (atom_t)NULL, TRUE)) &&
                  (mod = PL_new_module(mname), TRUE) &&
                  (predicate = PL_pred(func, mod), TRUE) &&
                  (rval = (*env)->AllocObject(env, jPredicateT_c)) != NULL &&
                  setPointerValue(env, rval, (pointer)predicate)
              ? (DEBUG(1, Sdprintf("[predicate() module=%s\n",
                                   (jmodule == NULL ? "(null)"
                                                    : PL_atom_chars(mname)))),
                 rval)
              : NULL /* oughta warn of failure? */
          );
}

/*
 * Class:         org_jpl7_fli_Prolog
 * Method:        put_float
 * Signature: (Lorg/jpl7/fli/term_t;D)V
 */
JNIEXPORT jboolean JNICALL
Java_org_jpl7_fli_Prolog_put_1float(JNIEnv *env, jclass jProlog, jobject jterm,
                                    jdouble jf)
{ term_t term;

  if ( jpl_ensure_pvm_init(env) &&
       getTermValue(env, jterm, &term) )
  { return PL_put_float(term, jf);
  }

  return FALSE;
}

/*
 * Class:         org_jpl7_fli_Prolog
 * Method:        put_integer
 * Signature: (Lorg/jpl7/fli/term_t;J)V
 */
JNIEXPORT jboolean JNICALL
Java_org_jpl7_fli_Prolog_put_1integer(JNIEnv *env, jclass jProlog,
                                      jobject jterm, jlong ji)
{ term_t term;

  if ( jpl_ensure_pvm_init(env) &&
       getTermValue(env, jterm, &term) )
  { return PL_put_int64(term, ji);
  }

  return FALSE;
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    put_integer_big
 * Signature: (Lorg/jpl7/fli/term_t;Ljava/lang/String;)V
 */
JNIEXPORT jboolean JNICALL
Java_org_jpl7_fli_Prolog_put_1integer_1big(JNIEnv *env, jclass jProlog,
                                           jobject jterm, jstring jvalue)
{ term_t term;

  if ( jpl_ensure_pvm_init(env) &&
       getTermValue(env, jterm, &term))
  { return PL_chars_to_term((char *)(*env)->GetStringUTFChars(env, jvalue, 0),
                            term);
  } else
  { return FALSE;
  }
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    put_nil
 * Signature: (Lorg/jpl7/fli/term_t;)V
 */
JNIEXPORT jboolean JNICALL
Java_org_jpl7_fli_Prolog_put_1nil(JNIEnv *env, // 1/Feb/2015
                                  jclass jProlog, jobject jterm)
{ term_t term;

  return ( jpl_ensure_pvm_init(env) &&
           getTermValue(env, jterm, &term) &&
           PL_put_nil(term) );
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    put_atom_chars
 * Signature: (Lorg/jpl7/fli/term_t;Ljava/lang/String)V
 */
JNIEXPORT jboolean JNICALL
Java_org_jpl7_fli_Prolog_put_1atom_1chars(
    JNIEnv *env,
    jclass jProlog, jobject jterm, jstring chars)
{ term_t term;
  atom_t a;

  return ( jpl_ensure_pvm_init(env) &&
           getTermValue(env, jterm, &term) &&
           jni_String_to_atom(env, chars, &a) &&
           PL_put_atom(term, a) &&
           (PL_unregister_atom(a),TRUE) );
}




/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    put_term
 * Signature: (Lorg/jpl7/fli/term_t;Lorg/jpl7/fli/term_t;)V
 */
JNIEXPORT void JNICALL /* maybe oughta return jboolean (false iff given object is null) */
Java_org_jpl7_fli_Prolog_put_1term(JNIEnv *env, jclass jProlog,
                                   jobject jterm1, jobject jterm2)
{ term_t term1;
  term_t term2;

  if ( jpl_ensure_pvm_init(env) &&
       getTermValue(env, jterm1, &term1) &&
       getTermValue(env, jterm2, &term2) )
  { PL_put_term(term1, term2);
  }
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    put_jref
 * Signature: (Lorg/jpl7/fli/term_t;Ljava/lang/Object;)V
 */
/* added 29/1/2007 PS to support restored but now deprecated org.jpl7.JRef for
 * Rick Moynihan */
JNIEXPORT void JNICALL
Java_org_jpl7_fli_Prolog_put_1jref(JNIEnv *env, jclass jProlog, jobject jterm,
                                   jobject jref)
{ term_t term;

  if ( jpl_ensure_pvm_init(env) &&
       jni_ensure_jvm() &&
       getTermValue(env, jterm, &term) )
  { jni_jobject_to_term(jref, term, env);       /* assumes term is var */
  }
}


/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    put_variable
 * Signature: (Lorg/jpl7/fli/term_t;)V
 */
JNIEXPORT void JNICALL
Java_org_jpl7_fli_Prolog_put_1variable(JNIEnv *env, jclass jProlog,
                                       jobject jterm)
{ term_t term;

  if ( jpl_ensure_pvm_init(env) &&
       getTermValue(env, jterm, &term) )
  { PL_put_variable(term);
  }
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    term_type
 * Signature: (Lorg/jpl7/fli/term_t;)I
 */
JNIEXPORT jint JNICALL
Java_org_jpl7_fli_Prolog_term_1type(JNIEnv *env, jclass jProlog, jobject jterm)
{ term_t term;

  if ( jpl_ensure_pvm_init(env) &&
       getTermValue(env, jterm, &term) )
    return PL_term_type(term);

  return -1;                                    /* i.e. when jterm is null */
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    unregister_atom
 * Signature: (Lorg/jpl7/fli/atom_t;)V
 */
JNIEXPORT void JNICALL
Java_org_jpl7_fli_Prolog_unregister_1atom(JNIEnv *env, jclass jProlog,
                                          jobject jatom)
{ atom_t atom;

  DEBUG(1, Sdprintf(">unregister_atom(env=%p,jProlog=%p,jatom=%p)...\n", env,
                    jProlog, jatom));

  if ( jpl_ensure_pvm_init(env) &&
       getAtomValue(env, jatom, &atom) )
  { PL_unregister_atom(atom);
    DEBUG(1, Sdprintf("  ok: PL_unregister_atom(%lu)\n", (long)atom));
  }
}

/*
 * Class:         org_jpl7_fli_Prolog
 * Method:        open_foreign_frame
 * Signature: ()Lorg/jpl7/fli/fid_t;
 */
JNIEXPORT jobject JNICALL
Java_org_jpl7_fli_Prolog_open_1foreign_1frame(JNIEnv *env, jclass jProlog)
{ jobject rval;

  if (jpl_ensure_pvm_init(env) &&
      (rval = (*env)->AllocObject(env, jFidT_c)) !=
          NULL // get a new fid_t object
      && setUIntPtrValue(
             env, rval,
             PL_open_foreign_frame()) // open a frame only if alloc succeeds
      )
  { return rval;
  } else
  { return NULL;
  }
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    discard_foreign_frame
 * Signature: (Lorg/jpl7/fli/fid_t;)V
 */
JNIEXPORT void JNICALL
Java_org_jpl7_fli_Prolog_discard_1foreign_1frame(JNIEnv *env, jclass jProlog,
                                                 jobject jfid)
{ fid_t fid;

  if ( jpl_ensure_pvm_init(env) &&
       getFIDValue(env, jfid, &fid) )
  { PL_discard_foreign_frame(fid);
  }
}

/*=== JPL's Prolog engine pool and thread management ====================== */

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    thread_self
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_org_jpl7_fli_Prolog_thread_1self(JNIEnv *env, jclass jProlog)
{ if ( jpl_ensure_pvm_init(env) )
  { return PL_thread_self();
  } else
  { return -2;
  }
}


static int
create_pool_engines(void)
{ int i;

  DEBUG(1, Sdprintf("JPL creating engine pool:\n"));
#ifdef USE_WIN_EVENTS
  engines_event = CreateEvent(NULL, FALSE, FALSE, NULL);
  InitializeCriticalSection(&engines_mutex);
#endif
  engines_allocated = JPL_MAX_POOL_ENGINES;
  if ((engines = malloc(sizeof(PL_engine_t) * engines_allocated)) == NULL)
    return -1;
  memset(engines, 0, sizeof(PL_engine_t) * engines_allocated);

  DEBUG(1, Sdprintf("JPL stashing default engine as [0]\n"));
  PL_set_engine(PL_ENGINE_CURRENT, &engines[0]);

  for (i = 1; i < JPL_INITIAL_POOL_ENGINES; i++)
  { if ( !(engines[i] = PL_create_engine(NULL)) )
      return -2;
    DEBUG(1, Sdprintf("\tengine[%d]=%p created\n", i, engines[i]));
  }
  return 0;
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    attach_pool_engine
 * Signature: ()Lorg/jpl7/fli/engine_t;
 */
#ifdef USE_WIN_EVENTS
#define LOCK_ENGINES() EnterCriticalSection(&engines_mutex)
#define UNLOCK_ENGINES() LeaveCriticalSection(&engines_mutex)
#else
#define LOCK_ENGINES() pthread_mutex_lock(&engines_mutex)
#define UNLOCK_ENGINES() pthread_mutex_unlock(&engines_mutex)
#endif

JNIEXPORT jobject JNICALL
Java_org_jpl7_fli_Prolog_attach_1pool_1engine(JNIEnv *env, jclass jProlog)
{ jobject rval;
  int     i;

  if ( !jpl_ensure_pvm_init(env) )
    return NULL;                                /* FIXME: throw exception */

  /* Find an engine. Try setting each of the engines in the pool. */
  /* If they are all in use wait for the condition variable and try again. */
  LOCK_ENGINES();
  for (;;)
  { try_again:
    for (i = 0; i < engines_allocated; i++)
    { int rc;

      if ( !engines[i] )
        continue;

      DEBUG(1, Sdprintf("JPL trying engine[%d]=%p\n", i, engines[i]));
      if ((rc = PL_set_engine(engines[i], NULL)) == PL_ENGINE_SET)
      { DEBUG(1, Sdprintf("JPL attaching engine[%d]=%p\n", i, engines[i]));
        UNLOCK_ENGINES();

        if ( (rval = (*env)->AllocObject(env, jEngineT_c)) )
        { setPointerValue(env, rval, (pointer)engines[i]);
          return rval;
        }

        PL_set_engine(NULL, NULL);
        return NULL;
      }
      if ( rc != PL_ENGINE_INUSE )
      { DEBUG(1, Sdprintf("JPL PL_set_engine %d fails with %d\n", i, rc));
        UNLOCK_ENGINES();
        return NULL; /* bad engine status: oughta throw exception */
      }
    }

    for (i = 0; i < engines_allocated; i++)
    { if ( !engines[i] )
      { if ( !(engines[i] = PL_create_engine(NULL)) )
	{ Sdprintf("JPL: Failed to create engine %d\n", i);
	  return NULL;
	}

	DEBUG(1, Sdprintf("JPL created engine[%d]=%p\n", i, engines[i]));

	goto try_again;
      }
    }

    DEBUG(1, Sdprintf("JPL no engines ready; waiting...\n"));
#ifdef USE_WIN_EVENTS
    UNLOCK_ENGINES();
    WaitForSingleObject(engines_event, 1000);
    LOCK_ENGINES();
#else
    pthread_cond_wait(&engines_cond, &engines_mutex);
#endif
  }
}

/* returns pool_index (0+) of given engine (else -1) */
static int
pool_engine_id(PL_engine_t e)
{ int i;

  for (i = 0; i < engines_allocated; i++)
  { if (engines[i] && engines[i] == e)
    { DEBUG(1,
            Sdprintf("JPL current  pool engine[%d] = %p (thread_self = %d)\n",
                     i, e, PL_thread_self()));
      return i;
    }
  }
  DEBUG(1, Sdprintf("JPL current non-pool engine = %p (thread_self = %d)\n", e,
                    PL_thread_self()));
  return -1; /* no current pool engine */
}

/* returns pool_index (0+) of attached engine (else -1), and writes its handle
 * into e */
static int
current_pool_engine_handle(PL_engine_t *e)
{ PL_set_engine(PL_ENGINE_CURRENT, e);
  return pool_engine_id(*e);
}

/* returns pool index (0+) of attached engine, else -1 */
static int
current_pool_engine(void)
{ PL_engine_t e;

  return current_pool_engine_handle(&e);
}

/* returns pool_index (0+) of given engine (else -1) */
/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    pool_engine_id
 * Signature: (Lorg/jpl7/fli/engine_t;)I
 */
JNIEXPORT int JNICALL
Java_org_jpl7_fli_Prolog_pool_1engine_1id(JNIEnv *env, jclass jProlog,
                                          jobject jengine)
{ PL_engine_t engine;

  if (!jpl_ensure_pvm_init(env))
  { return -2; /* libpl could not be initialised (oughta throw exception) */
  }
  if (!getPointerValue(env, jengine,
                       (intptr_t *)&engine)) /* checks jengine isn't null */
  { return -3; /* null engine holder */
  }
  return pool_engine_id(engine);
}

/*
 * Class:     org_jpl7_fli_Prolog
 * Method:    release_pool_engine
 * Signature: ()I
 */
JNIEXPORT int JNICALL
Java_org_jpl7_fli_Prolog_release_1pool_1engine(JNIEnv *env, jclass jProlog)
{ /* Detach our engine, making it available to the pool. */
  /* Signal the condition variable as there may be threads waiting for an
   * engine. */

  if (jpl_ensure_pvm_init(env))
  { int         i;
    PL_engine_t e;

    i = current_pool_engine_handle(&e);
    if (i > 0)
    { DEBUG(1, Sdprintf("JPL releasing engine[%d]=%p\n", i, e));
      LOCK_ENGINES();
      PL_set_engine(NULL, NULL);
#ifdef USE_WIN_EVENTS
      SetEvent(engines_event);
#else
      pthread_cond_signal(&engines_cond); /* alert waiters */
#endif
      UNLOCK_ENGINES();
    }
    return i;
  } else
  { return -2;
  }
}

static foreign_t
jni_term_to_jref_plc(term_t tref1, /* +term: AnyPrologTerm */
                     term_t tref2  /* -term: JRef to a org.jpl7.Term instance
                                      which represents that term */
                     )
{ jobject term1;
  JNIEnv *env;

  return jni_ensure_jvm()            /* untypically... */
         && jpl_ensure_pvm_init(env) /* ...this requires both inits */
         && (term1 = (*env)->AllocObject(env, termt_class)) != NULL &&
         setUIntPtrValue(
             env, term1,
             tref1) /* requires jLongHolderValue_f to be initialised */
         && jni_jobject_to_term((*env)->CallStaticObjectMethod(
                                    env, term_class, term_getTerm, term1),
                                tref2, env) &&
         jni_check_exception(env);
}

/* serves jni_jref_to_term_plc() */
static bool
jni_jobject_to_term_byval(
    JNIEnv *env, jobject jobj, /* this must be an instance of one of
                                  org.jpl7.Term's subclasses */
    term_t term /* a Prolog term, as represented by jobj, is *put* into this
                   term ref */
    )
{ jobject termt; /* a temporary instance of org.jpl7.fli.term_t (i.e. a "term
                    holder") */

  return /* jni_ensure_jvm() && jpl_ensure_pvm_init(env) && */
      (termt = (*env)->AllocObject(env, termt_class)) != NULL &&
      setUIntPtrValue(env, termt,
                      term) /* requires jLongHolderValue_f to be initialised */
      &&
      ((*env)->CallStaticVoidMethod(env, term_class, term_putTerm, jobj, termt),
       TRUE) &&
      jni_check_exception(env);
}

/* if the first arg is a <jref> which refers to a org.jpl7.Term instance, */
/* then the 2nd arg will be matched with a corresponding newly constructed term
 */
static foreign_t
jni_jref_to_term_plc(term_t jref,  /* +term: JRef to a org.jpl7.Term instance */
                     term_t termIn /* -term: term as represented by the JRef */
                     )
{ atom_t a;
  term_t term =
      PL_new_term_ref(); /* jni_jobject_to_term_byval() will *put* the
                            constructed term in here */
  JNIEnv *     env;
  PL_blob_t *  type;
  jref_handle *ref;

  return jni_ensure_jvm()            /* untypically... */
         && jpl_ensure_pvm_init(env) /* ...this requires both inits */
         && PL_get_atom(jref, &a) && (ref = PL_blob_data(a, NULL, &type)) &&
         type == &jref_blob &&
         jni_jobject_to_term_byval(env, (jobject)(ref->iref), term) &&
         PL_unify(termIn, term) /* attempt to unify the 2nd arg with the newly
                                   constructed term */
      ;
}

static bool
jni_get_default_jvm_opts_1(term_t args, int i, char **jvm_xia)
{ term_t tp = PL_new_term_ref();

  if (jvm_xia[i] == NULL)
  { return PL_unify_nil(args);
  } else
  { return PL_unify_list(args, tp, args) &&
           PL_unify_term(tp, PL_ATOM, PL_new_atom(jvm_xia[i])) &&
           jni_get_default_jvm_opts_1(args, i + 1, jvm_xia);
  }
}

static foreign_t
jni_get_jvm_opts(term_t args, /* -list(atom): the current default JVM
                                 initialisation options */
                 char **jvm_xia)
{ if (jvm_xia == NULL)
  { return FALSE;
  } else
  { return jni_get_default_jvm_opts_1(args, 0, jvm_xia);
  }
}

static foreign_t
jni_set_default_jvm_opts_plc(
    term_t tn, /* +integer: the qty of options */
    term_t
        args /* +list(atom): the current default JVM initialisation options */
    )
{ int    n;
  int    i;
  term_t head;
  term_t list;
  char * s;

  if (jvm_dia == NULL) /* presumably, JVM is already started, so default options
                          cannot now be set */
  { return FALSE;
  }
  if (!PL_get_integer(tn, &n)) /* arg is not an integer (shouldn't happen: our
                                  code passes length of list */
  { return FALSE;
  }
  if (jvm_dia ==
      jvm_ia) /* jvm_dia still points to the built-in (non-malloc-ed) default
                 default opts */
  { DEBUG(1, Sdprintf("JPL not freeing original (static) JVM opts; replacing "
                      "with malloc-ed [%d+1]\n",
                      n));
    jvm_dia = (char **)malloc((n + 1) * sizeof(char **));
  } else
  { DEBUG(1, Sdprintf("JPL has malloc-ed JVM opt[?] (of malloc-ed strings)\n"));
    for (i = 0; jvm_dia[i] != NULL && i < 100;
         i++) /* a malloc-ed array always has NULL in its last element */
    { DEBUG(1, Sdprintf("JPL freeing malloc-ed JVM opt '%s'\n", jvm_dia[i]));
      free(jvm_dia[i]); /* a malloc-ed array's elements always point to
                           malloc-ed strings, which we should free */
    }
    if (n != i) /* we need an array of a different length */
    { DEBUG(1, Sdprintf("JPL needs different qty JVM opts so freeing old [%d] "
                        "and malloc-ing new [%d]\n",
                        i, n));
      free(jvm_dia);
      jvm_dia = (char **)malloc((n + 1) * sizeof(char **));
    } else
    { DEBUG(1, Sdprintf("JPL needs [%d] JVM opts as before\n"));
    }
  }
  head = PL_new_term_ref();      /* variable for the elements */
  list = PL_copy_term_ref(args); /* copy as we need to write */
  for (i = 0; PL_get_list(list, head, list); i++)
  { if (PL_get_atom_chars(head, &s))
    { DEBUG(1, Sdprintf("JPL malloc-ing space for '%s'\n", s));
      jvm_dia[i] = (char *)malloc(strlen(s) + 1);
      strcpy(jvm_dia[i], s);
    } else
    { return FALSE;
    }
  }
  jvm_dia[i] = NULL;       /* stash a sentinel */
  return PL_get_nil(list); /* succeed iff list is proper */
}

static foreign_t
jni_get_default_jvm_opts_plc(
    term_t args /* -list(atom): the current default JVM initialisation options */
    )
{ return jni_get_jvm_opts(args, jvm_dia);
}

static foreign_t
jni_get_actual_jvm_opts_plc(
    term_t args /* -list(atom): the actual JVM initialisation options */
    )
{ return jni_get_jvm_opts(args, jvm_aia);
}

static int
jpl_get_syntax(JNIEnv *env)
{ if (jpl_syntax == JPL_SYNTAX_UNDEFINED && jpl_ensure_pvm_init(env))
  { jpl_syntax = (ATOM_nil == PL_new_atom("[]") ? JPL_SYNTAX_TRADITIONAL
                                                : JPL_SYNTAX_MODERN);
  }
  return jpl_syntax;
}

/*
 * Class:         org_jpl7_fli_Prolog
 * Method:    get_syntax
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_org_jpl7_fli_Prolog_get_1syntax(JNIEnv *env, jclass jProlog)
{ return jpl_get_syntax(env);
}

/*=== FLI metadata ======================================================== */

static PL_extension predspecs[] = {
  {"jni_get_created_jvm_count",    1, jni_get_created_jvm_count_plc,    0},
  {"jni_ensure_jvm",               0, jni_ensure_jvm_plc,               0},
  {"jni_tag_to_iref",              2, jni_tag_to_iref_plc,              0},
  {"jni_hr_info",                  4, jni_hr_info_plc,                  0},
  {"jni_hr_table",                 1, jni_hr_table_plc,                 0},
  {"jni_byte_buf_length_to_codes", 3, jni_byte_buf_length_to_codes_plc, 0},
  {"jni_param_put",                4, jni_param_put_plc,                0},
  {"jni_alloc_buffer",             3, jni_alloc_buffer_plc,             0},
  {"jni_free_buffer",              1, jni_free_buffer_plc,              0},
  {"jni_fetch_buffer_value",       4, jni_fetch_buffer_value_plc,       0},
  {"jni_stash_buffer_value",       4, jni_stash_buffer_value_plc,       0},
  {"jni_void",                     1, jni_void_0_plc,                   0},
  {"jni_void",                     2, jni_void_1_plc,                   0},
  {"jni_void",                     3, jni_void_2_plc,                   0},
  {"jni_void",                     4, jni_void_3_plc,                   0},
  {"jni_void",                     5, jni_void_4_plc,                   0},
  {"jni_func",                     2, jni_func_0_plc,                   0},
  {"jni_func",                     3, jni_func_1_plc,                   0},
  {"jni_func",                     4, jni_func_2_plc,                   0},
  {"jni_func",                     5, jni_func_3_plc,                   0},
  {"jni_func",                     6, jni_func_4_plc,                   0},
  {"jpl_c_lib_version",            1, jpl_c_lib_version_1_plc,          0},
  {"jpl_c_lib_version",            4, jpl_c_lib_version_4_plc,          0},
  {"jni_term_to_jref",             2, jni_term_to_jref_plc,             0},
  {"jni_jref_to_term",             2, jni_jref_to_term_plc,             0},
  {"jni_get_default_jvm_opts",     1, jni_get_default_jvm_opts_plc,     0},
  {"jni_set_default_jvm_opts",     2, jni_set_default_jvm_opts_plc,     0},
  {"jni_get_actual_jvm_opts",      1, jni_get_actual_jvm_opts_plc,      0},
  {NULL,                           0, NULL,                             0}
};

install_t
install(void)
{ PL_register_extensions(predspecs);
#if _YAP_NOT_INSTALLED_
  ATOM_nil = PL_new_atom("[]");
  jvm = NULL;
#endif
  }

/*=== end of jpl.c ======================================================== */
