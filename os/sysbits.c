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
* File:		sysbits.c *
* Last rev:	4/03/88							 *
* mods: *
* comments:	very much machine dependent routines			 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

#include "sysbits.h"

/// File Error Handler
static void FileError(yap_error_number type, Term where, const char *format,
                      ...) {

  if (trueLocalPrologFlag(FILEERRORS_FLAG)) {
    va_list ap;

    va_start(ap, format);
    /* now build the error string */
    Yap_Error(type, TermNil, format, ap);
    va_end(ap);
  }
}

/// Allocate a temporary buffer
static char *getFileNameBuffer(void) {

  return Yap_AllocAtomSpace( YAP_FILENAME_MAX );
}

static void freeFileNameBuffer(char *s)
{
  Yap_FreeCodeSpace(s);
}

static Int p_sh(USES_REGS1);
static Int p_shell(USES_REGS1);
static Int p_system(USES_REGS1);
static Int p_mv(USES_REGS1);
static Int p_dir_sp(USES_REGS1);
static Int p_getenv(USES_REGS1);
static Int p_putenv(USES_REGS1);
static Term do_glob(const char *spec, bool ok_to);
#ifdef MACYAP

static int chdir(char *);
/* #define signal	skel_signal */
#endif /* MACYAP */
static const char *expandVars(const char *spec, char *u);

void exit(int);


#ifdef _WIN32
void Yap_WinError(char *yap_error) {
  char msg[256];
  /* Error, we could not read time */
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                NULL, GetLastError(), MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                msg, 256, NULL);
  Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, TermNil, "%s at %s", msg, yap_error);
}
#endif /* __WINDOWS__ */

#define is_valid_env_char(C)                                                   \
  (((C) >= 'a' && (C) <= 'z') || ((C) >= 'A' && (C) <= 'Z') || (C) == '_')



/// is_directory: verifies whether an expanded file name
/// points at a readable directory
static bool is_directory(const char *FileName) {

  VFS_t *vfs;
  if ((vfs = vfs_owner( FileName ))) {
    return vfs->isdir( vfs, FileName );
  }
#ifdef __WINDOWS__
  DWORD dwAtts = GetFileAttributes(FileName);
  if (dwAtts == INVALID_FILE_ATTRIBUTES)
    return false;
  return (dwAtts & FILE_ATTRIBUTE_DIRECTORY);
#elif HAVE_LSTAT
  struct stat buf;

  if (lstat(FileName, &buf) == -1) {
    /* return an error number */
    return false;
  }
  return S_ISDIR(buf.st_mode);
#else
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
            "stat not available in this configuration");
  return false;
#endif
}

bool Yap_Exists(const char *f) {
  VFS_t *vfs;

  if ((vfs = vfs_owner( f ))) {
    return vfs->exists( vfs, f );
  }
#if _WIN32
  if (_access(f, 0) == 0)
    return true;
  if (errno == EINVAL) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "bad flags to access");
  }
  return false;
#elif HAVE_ACCESS
  if (access(f, F_OK) == 0)
    return true;
  if (errno == EINVAL) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "bad flags to access");
  }
  return false;
#else
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
            "access not available in this configuration");
  return false;
#endif
}

static int dir_separator(int ch) {
#ifdef MAC
  return (ch == ':');
#elif ATARI || _MSC_VER
  return (ch == '\\');
#elif defined(__MINGW32__) || defined(__CYGWIN__)
  return (ch == '\\' || ch == '/');
#else
  return (ch == '/');
#endif
}

int Yap_dir_separator(int ch) { return dir_separator(ch); }

#if __WINDOWS__
#include <psapi.h>

char *libdir = NULL;
#endif

bool Yap_IsAbsolutePath(const char *p0) {
  // verify first if expansion is needed: ~/ or $HOME/
  const char *p = expandVars(p0, LOCAL_FileNameBuf);
  bool nrc;
#if _WIN32 || __MINGW32__
  nrc = !PathIsRelative(p);
#else
  nrc = (p[0] == '/');
#endif
  return nrc;
}

#define isValidEnvChar(C)                                                      \
  (((C) >= 'a' && (C) <= 'z') || ((C) >= 'A' && (C) <= 'Z') || (C) == '_')

// this is necessary because
// support for ~expansion at the beginning
// systems like Android do not do this.
static const char *PlExpandVars(const char *source, const char *root,
                                char *result) {
  CACHE_REGS
  const char *src = source;
  if (!result)
    result = malloc(YAP_FILENAME_MAX + 1);

  if (strlen(source) >= YAP_FILENAME_MAX) {
    Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, TermNil,
              "%s in true_file-name is larger than the buffer size (%d bytes)",
              source, strlen(source));
  }
  /* step 1: eating home information */
  if (source[0] == '~') {
    if (dir_separator(source[1]) || source[1] == '\0') {
      char *s;
      src++;
#if defined(_WIN32)
      s = getenv("HOMEDRIVE");
      if (s != NULL)
        strncpy(result, getenv("HOMEDRIVE"), YAP_FILENAME_MAX);
// s = getenv("HOMEPATH");
#else
      s = getenv("HOME");
#endif
      if (s != NULL)
        strncpy(result, s, YAP_FILENAME_MAX);
      strcat(result, src);
      return result;
    } else {
#if HAVE_GETPWNAM
      struct passwd *user_passwd;
      char *res = result;

      src++;
      while (!dir_separator((*res = *src)) && *res != '\0')
        res++, src++;
      res[0] = '\0';
      if ((user_passwd = getpwnam(result)) == NULL) {
        FileError(SYSTEM_ERROR_OPERATING_SYSTEM,
                  MkAtomTerm(Yap_LookupAtom(source)),
                  "User %s does not exist in %s", result, source);
        return NULL;
      }
      strncpy(result, user_passwd->pw_dir, YAP_FILENAME_MAX);
      strcat(result, src);
#else
      FileError(
          SYSTEM_ERROR_OPERATING_SYSTEM, MkAtomTerm(Yap_LookupAtom(source)),
          "User %s cannot be found in %s, missing getpwnam", result, source);
      return NULL;
#endif
    }
    return result;
  }
  // do VARIABLE expansion
  else if (source[0] == '$') {
    /* follow SICStus expansion rules */
    char v[YAP_FILENAME_MAX + 1];
    int ch;
    char *s, *res;
    src = source + 1;
    if (src[0] == '{') {
      res = v;
      src++;
      while ((*res = (ch = *src++)) && isValidEnvChar(ch) && ch != '}') {
        res++;
      }
      if (ch == '}') {
        // {...}
        // done
        res[0] = '\0';
      }
    } else {
      res = v;
      while ((*res = (ch = *src++)) && isValidEnvChar(ch) && ch != '}') {
        res++;
      }
      src--;
      res[0] = '\0';
    }
    if ((s = (char *)getenv(v))) {
      strcpy(result, s);
      strcat(result, src);
    } else
      strcpy(result, src);
  } else {
    size_t tocp = strlen(src);
    if (root) {
      tocp = strlen(root) + 1;
    }
    if (tocp > YAP_FILENAME_MAX) {
      Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, MkStringTerm(src),
                "path too long");
      return NULL;
    }
    if (root && !Yap_IsAbsolutePath(source)) {
      strncpy(result, root, YAP_FILENAME_MAX);
      if (root[strlen(root)-1] != '/')
	strncat(result, "/", YAP_FILENAME_MAX);
      strncat(result, source, YAP_FILENAME_MAX);
    } else {
      strncpy(result, source, strlen(src) + 1);
    }
  }
  return result;
}

#if _WIN32
// straightforward conversion from Unix style to WIN style
// check cygwin path.cc for possible improvements
static char *unix2win(const char *source, char *target, int max) {
  char *s = target;
  const char *s0 = source;
  char *s1;
  int ch;

  if (s == NULL)
    s = malloc(YAP_FILENAME_MAX + 1);
  s1 = s;
  // win32 syntax
  // handle drive notation, eg //a/
  if (s0[0] == '\0') {
    s[0] = '.';
    s[1] = '\0';
    return s;
  }
  if (s0[0] == '/' && s0[1] == '/' && isalpha(s0[2]) && s0[3] == '/') {
    s1[0] = s0[2];
    s1[1] = ':';
    s1[2] = '\\';
    s0 += 4;
    s1 += 3;
  }
  while ((ch = *s1++ = *s0++)) {
    if (ch == '$') {
      s1[-1] = '%';
      ch = *s0;
      // handle $(....)
      if (ch == '{') {
        s0++;
        while ((ch = *s0++) != '}') {
          *s1++ = ch;
          if (ch == '\0')
            return FALSE;
        }
        *s1++ = '%';
      } else {
        while (((ch = *s1++ = *s0++) >= 'A' && ch <= 'Z') ||
               (ch >= 'a' && ch <= 'z') || (ch == '-') ||
               (ch >= '0' && ch <= '9') || (ch == '_'))
          ;
        s1[-1] = '%';
        *s1++ = ch;
        if (ch == '\0') {
          s1--;
          s0--;
        }
      }
    } else if (ch == '/')
      s1[-1] = '\\';
  }
  return s;
}
#endif

static char *OsPath(const char *p, char *buf) { return (char *)p; }

static char *PrologPath(const char *Y, char *X) { return (char *)Y; }

#if _WIN32
#define HAVE_BASENAME 1
#define HAVE_REALPATH 1
#endif

static bool ChDir(const char *path) {
  bool rc = false;
  char qp[FILENAME_MAX + 1];
  const char *qpath = Yap_AbsoluteFile(path, qp, true);

VFS_t *v;
    if ((v = vfs_owner( path ))) {
       return v->chdir(v, path);
    }
#if _WIN32
  rc = true;
  if (qpath != NULL && qpath[0] &&
      (rc = (SetCurrentDirectory(qpath) != 0)) == 0) {
    Yap_WinError("SetCurrentDirectory failed");
  }
#else
  rc = (chdir(qpath) == 0);
#endif
  if (qpath != qp && qpath != LOCAL_FileNameBuf && qpath != LOCAL_FileNameBuf2)
    free((char *)qpath);
  return rc;
}

static const char *myrealpath(const char *path, char *out) {
  if (!out)
    out = LOCAL_FileNameBuf;
#if _WIN32
  DWORD retval = 0;

  // notice that the file does not need to exist
  retval = GetFullPathName(path, YAP_FILENAME_MAX, out, NULL);
  if (retval == 0) {
    Yap_WinError("Generating a full path name for a file");
    return NULL;
  }
  return out;
#elif HAVE_REALPATH
  {
    char *rc = realpath(path, NULL);

    if (rc) {
      return rc;
    }
    // rc = NULL;
    if (errno == ENOENT || errno == EACCES) {
      char base[YAP_FILENAME_MAX + 1];
      strncpy(base, path, YAP_FILENAME_MAX - 1);
      rc = realpath(dirname(base), out);

      if (rc) {
        // base may haave been destroyed
        const char *b = basename((char *)path);
        size_t e = strlen(rc);
        size_t bs = strlen(b);

        if (rc != out && rc != base) {
          rc = realloc(rc, e + bs + 2);
        }
#if _WIN32
        if (rc[e - 1] != '\\' && rc[e - 1] != '/') {
          rc[e] = '\\';
          rc[e + 1] = '\0';
        }
#else
        if (rc[e - 1] != '/') {
          rc[e] = '/';
          rc[e + 1] = '\0';
        }
#endif
        strcat(rc, b);
        return rc;
      }
    }
  }
#endif
  out = malloc(strlen(path) + 1);
  strcpy(out, path);
  return out;
}

static const char *expandVars(const char *spec, char *u) {
  CACHE_REGS
#if _WIN32 || defined(__MINGW32__)
  char *out;

  // first pass, remove Unix style stuff
  if ((out = unix2win(spec, u, YAP_FILENAME_MAX)) == NULL)
    return NULL;
  spec = u;
#endif
  bool ok_to = LOCAL_PrologMode && !(LOCAL_PrologMode & BootMode);
  if (ok_to) {
    Term t = do_glob(spec, true);
    if (IsPairTerm(t))
      return RepAtom(AtomOfTerm(HeadOfTerm(t)))->StrOfAE;
    return NULL;
  }
  return spec;
}

/**
 * generate absolute path, if ok first expand SICStus Prolog style
 *
 * @param[in]  spec the file path, including `~` and `$`.
 * @param[in]  ok where to process `~` and `$`.
 *
 * @return tmp, or NULL, in malloced memory
 */
const char *Yap_AbsoluteFile(const char *spec, char *rc0, bool ok) {
  const char *rc;
  const char *spec1;
  const char *spec2;
  char rc1[YAP_FILENAME_MAX + 1];

/// spec gothe original spec;
/// rc0 may be an outout buffer
/// rc1 the internal buffer
///
/// PlExpandVars

#if _WIN32
  char rc2[YAP_FILENAME_MAX];
  if ((rc = unix2win(spec, rc2, YAP_FILENAME_MAX)) == NULL) {
    return NULL;
  }
  spec1 = rc;
#else
  spec1 = spec;
#endif
  /// spec gothe original spec;
  /// rc1 the internal buffer
  if (ok) {
    const char *q = PlExpandVars(spec1, NULL, rc1);
    if (!q)
      spec2 = spec1;
    else
      spec2 = q;
  } else {
    spec2 = spec1;
  }
  rc = myrealpath(spec2, rc0);
  return rc;
}

static Term
    /* Expand the string for the program to run.  */
    do_glob(const char *spec, bool glob_vs_wordexp) {
  CACHE_REGS
  if (spec == NULL) {
    return TermNil;
  }
#if _WIN32
  char u[YAP_FILENAME_MAX + 1];
  const char *espec = u;
  {
    WIN32_FIND_DATA find;
    HANDLE hFind;
    CELL *dest;
    Term tf;
    char drive[_MAX_DRIVE];
    char dir[_MAX_DIR];
    char fname[_MAX_FNAME];
    char ext[_MAX_EXT];

    _splitpath(spec, drive, dir, fname, ext);
    _makepath(u, drive, dir, fname, ext);

    // first pass, remove Unix style stuff
    hFind = FindFirstFile(u, &find);
    if (hFind == INVALID_HANDLE_VALUE) {
      return TermNil;
    } else {
      tf = AbsPair(HR);
      _makepath(u, drive, dir, find.cFileName, NULL);
      HR[0] = MkAtomTerm(Yap_LookupAtom(u));
      HR[1] = TermNil;
      dest = HR + 1;
      HR += 2;
      while (FindNextFile(hFind, &find)) {
        *dest = AbsPair(HR);
        _makepath(u, drive, dir, find.cFileName, NULL);
        HR[0] = MkAtomTerm(Yap_LookupAtom(u));
        HR[1] = TermNil;
        dest = HR + 1;
        HR += 2;
      }
      FindClose(hFind);
    }
    return tf;
  }
#elif HAVE_WORDEXP || HAVE_GLOB
  char u[YAP_FILENAME_MAX + 1];
  const char *espec = u;
  strncpy(u, spec, sizeof(u));
  /* Expand the string for the program to run.  */
  size_t pathcount;
#if HAVE_GLOB
  glob_t gresult;
#endif
#if HAVE_WORDEXP
  wordexp_t wresult;
#endif
#if HAVE_GLOB || HAVE_WORDEXP
  char **ss = NULL;
  int flags = 0, j;
#endif
  if (glob_vs_wordexp) {
#if HAVE_GLOB
#ifdef GLOB_NOCHECK
    flags = GLOB_NOCHECK;
#else
    flags = 0;
#endif
#ifdef GLOB_BRACE
    flags |= GLOB_BRACE | GLOB_TILDE;
#endif
    switch (glob(espec, flags, NULL, &gresult)) {
    case 0: /* Successful.  */
      ss = gresult.gl_pathv;
      pathcount = gresult.gl_pathc;
      if (pathcount) {
        break;
      }
    case GLOB_NOMATCH:
      globfree(&gresult);
      { return TermNil; }
    case GLOB_ABORTED:
      PlIOError(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1, "glob aborted: %sn",
                strerror(errno));
      globfree(&gresult);
      return TermNil;
    case GLOB_NOSPACE:
      Yap_Error(RESOURCE_ERROR_HEAP, ARG1, "glob ran out of space: %sn",
                strerror(errno));
      globfree(&gresult);
      return TermNil;
    /* If the error was WRDE_NOSPACE,
     then perhaps part of the result was allocated.  */
    default: /* Some other error.  */
      return TermNil;
    }
#endif
  } else {
#if HAVE_WORDEXP
    int rc;
    memset(&wresult, 0, sizeof(wresult));
    switch ((rc = wordexp(espec, &wresult, flags))) {
    case 0: /* Successful.  */
      ss = wresult.we_wordv;
      pathcount = wresult.we_wordc;
      if (pathcount) {
        break;
      } else {
        Term t;
        t = MkAtomTerm(Yap_LookupAtom(expandVars(espec, NULL)));
        wordfree(&wresult);
        return MkPairTerm(t, TermNil);
      }
    case WRDE_NOSPACE:
      /* If the error was WRDE_NOSPACE,
       then perhaps part of the result was allocated.  */
      Yap_Error(RESOURCE_ERROR_HEAP, ARG1, "wordexp ran out of space: %s",
                strerror(errno));
      wordfree(&wresult);
      return TermNil;
    default: /* Some other error.  */
      PlIOError(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1, "wordexp failed: %s",
                strerror(errno));
      wordfree(&wresult);
      return TermNil;
    }
#endif
  }
  const char *tmp;
  Term tf = TermNil;
  for (j = 0; j < pathcount; j++) {
    const char *s = ss[pathcount - (j + 1)];
    tmp = s;
    // if (!exists(s))
    //  continue;
    Atom a = Yap_LookupAtom(tmp);
    tf = MkPairTerm(MkAtomTerm(a), tf);
  }
#if HAVE_GLOB
  if (glob_vs_wordexp)
    globfree(&gresult);
#endif
#if HAVE_WORDEXP
  if (!glob_vs_wordexp)
    wordfree(&wresult);
#endif
  return tf;
#else
  // just use basic
  return MkPairTerm(MkAtomTerm(Yap_LookupAtom(spec)), TermNil);
#endif
}

/**
 * @pred prolog_expanded_file_system_path( +PrologPath, +ExpandVars, -OSPath )
 *
 * Apply basic transformations to paths, and conidtionally apply
 * traditional SICStus-style variable expansion.
 *
 * @param PrologPath the source, may be atom or string
 * @param ExpandVars expand initial occurrence of ~ or $
 * @param Prefix add this path before _PrologPath_
 * @param OSPath pathname.
 *
 * @return
 */
static Int real_path(USES_REGS1) {
  Term t1 = Deref(ARG1);
  const char *cmd, *rc0;

  if (IsAtomTerm(t1)) {
    cmd = RepAtom(AtomOfTerm(t1))->StrOfAE;
  } else if (IsStringTerm(t1)) {
    cmd = StringOfTerm(t1);
  } else {
    return false;
  }
#if _WIN32
  char cmd2[YAP_FILENAME_MAX + 1];
  char *rc;

  if ((rc = unix2win(cmd, cmd2, YAP_FILENAME_MAX)) == NULL) {
    return false;
  }
  cmd = rc;
#endif

  rc0 = myrealpath(cmd, NULL);
  if (!rc0) {
    PlIOError(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1, NULL);
  }
  bool out = Yap_unify(MkAtomTerm(Yap_LookupAtom(rc0)), ARG2);
  freeBuffer(rc0);
  return out;
}

#define EXPAND_FILENAME_DEFS()                                                 \
  PAR("parameter_expansion", isatom, EXPAND_FILENAME_PARAMETER_EXPANSION)      \
  , PAR("commands", booleanFlag, EXPAND_FILENAME_COMMANDS),                    \
      PAR(NULL, ok, EXPAND_FILENAME_END)

#define PAR(x, y, z) z

typedef enum expand_filename_enum_choices {
  EXPAND_FILENAME_DEFS()
} expand_filename_enum_choices_t;

#undef PAR

#define PAR(x, y, z)                                                           \
  { x, y, z }

static const param_t expand_filename_defs[] = {EXPAND_FILENAME_DEFS()};
#undef PAR

static Term do_expand_file_name(Term t1, Term opts USES_REGS) {
  xarg *args;
  expand_filename_enum_choices_t i;
  bool use_system_expansion = true;
  const char *tmpe = NULL;
  const char *spec;
  Term tf;

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, NULL);
    return TermNil;
  } else if (IsAtomTerm(t1)) {
    spec = AtomTermName(t1);
  } else if (IsStringTerm(t1)) {
    spec = StringOfTerm(t1);
  } else {
    Yap_Error(TYPE_ERROR_ATOM, t1, NULL);
    return TermNil;
  }
#if _WIN32
  {
    char *rc;
    char cmd2[YAP_FILENAME_MAX + 1];

    if ((rc = unix2win(spec, cmd2, YAP_FILENAME_MAX)) == NULL) {
      return false;
    }
    spec = rc;
  }
#endif

  args = Yap_ArgListToVector(opts, expand_filename_defs, EXPAND_FILENAME_END);
  if (args == NULL) {
    return TermNil;
  }
  tmpe = NULL;

  for (i = 0; i < EXPAND_FILENAME_END; i++) {
    if (args[i].used) {
      Term t = args[i].tvalue;
      switch (i) {
      case EXPAND_FILENAME_PARAMETER_EXPANSION:
        if (t == TermProlog) {
          tmpe = expandVars(spec, LOCAL_FileNameBuf);
          if (tmpe == NULL) {
            free(args);
            return TermNil;
          }
          spec = tmpe;
        } else if (t == TermTrue) {
          use_system_expansion = true;
        } else if (t == TermFalse) {
          use_system_expansion = false;
        }
        break;
      case EXPAND_FILENAME_COMMANDS:
        if (!use_system_expansion) {
          use_system_expansion = true;
#if 0 // def WRDE_NOCMD
                    if (t == TermFalse) {
                        flags = WRDE_NOCMD;
                    }
#endif
        }
      case EXPAND_FILENAME_END:
        break;
      }
    }
  }
  free(args);
  if (!use_system_expansion) {
    const char *o = expandVars(spec, NULL);
    if (!o)
      return false;
    tf = MkPairTerm(MkAtomTerm(Yap_LookupAtom(o)), TermNil);
#if _WIN32
    if (o != cmd2)
#endif
    {
      freeBuffer(o);
    }
  } else {
    tf = do_glob(spec, true);
  }
  if (tmpe
#if _WIN32
      && tmpe != cmd2
#endif
      ) {
    freeBuffer(tmpe);
  }
  return tf;
}

/* @pred expand_file_name( +Pattern, -ListOfPaths) is det

This builtin receives a pattern and expands it into a list of files.
  In Unix-like systems, YAP applies glob to expand patterns such as '*', '.',
and '?'. Further variable expansion
may also happen. glob is shell-dependent: som   Yap_InitCPred
("absolute_file_system_path", 2, absolute_file_system_path, 0);
    Yap_InitCPred ("real_path", 2, prolog_realpath, 0);
    Yap_InitCPred ("true_file_name", 2,
                   true_file_name, SyncPredFlag);
    Yap_InitCPred ("true_file_name", 3, true_file_name3, SyncPredFlag);
e shells allow command execution and brace-expansion.

*/
static Int expand_file_name(USES_REGS1) {
  Term tf = do_expand_file_name(Deref(ARG1), TermNil PASS_REGS);
  if (tf == 0)
    return false;
  return Yap_unify(tf, ARG2);
}

static Int expand_file_name3(USES_REGS1) {
  Term tf = do_expand_file_name(Deref(ARG1), Deref(ARG2) PASS_REGS);
  return Yap_unify(tf, ARG3);
}

/*
       static char *canoniseFileName( char *path) {
       #if HAVE_REALPATH && HAVE_BASENAME
       #if _WIN32 || defined(__MINGW32__)
         char *o = malloc(YAP_FILENAME_MAX+1);
         if (!o)
           return NULL;
         // first pass, remove Unix style stuff
         if (unix2win(path, o, YAP_FILENAME_MAX) == NULL)
           return NULL;
         path = o;
       #endif
         char *rc;
         if (tmp == NULL) return NULL;
         rc = myrealpath(path);
       #if _WIN32 || defined(__MINGW32__)
         freeBuffer(o);
#endif
  return rc;
#endif
}
*/

static Int absolute_file_system_path(USES_REGS1) {
  Term t = Deref(ARG1);
  const char *fp;
  bool rc;
  char s[MAXPATHLEN + 1];
  const char *text = Yap_TextTermToText(t, s, MAXPATHLEN, LOCAL_encoding);

  if (text == NULL) {
    return false;
  }
  if (!(fp = Yap_AbsoluteFile(RepAtom(AtomOfTerm(t))->StrOfAE, NULL, true)))
    return false;
  rc = Yap_unify(Yap_MkTextTerm(fp, LOCAL_encoding, t), ARG2);
  if (fp != s)
    freeBuffer((void *)fp);
  return rc;
}

static Int prolog_to_os_filename(USES_REGS1) {
  Term t = Deref(ARG1), t2 = Deref(ARG2);
  char *fp;
  char out[MAXPATHLEN + 1];

  if (IsVarTerm(t)) {

    if (IsVarTerm(t2)) {
      Yap_Error(INSTANTIATION_ERROR, t, "prolog_to_os_filename");
      return false;
    } else if (IsAtomTerm(t2)) {
      if (!(fp = PrologPath(RepAtom(AtomOfTerm(t2))->StrOfAE, out)))
        return false;
      return Yap_unify(ARG1, MkAtomTerm(Yap_LookupAtom(fp)));
    } else {
      Yap_Error(TYPE_ERROR_ATOM, t2, "prolog_to_os_filename");
      return false;
    }
  } else if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM, t, "prolog_to_os_filename");
    return false;
  }

  if (!(fp = OsPath(RepAtom(AtomOfTerm(t))->StrOfAE, out)))
    return false;
  return Yap_unify(MkAtomTerm(Yap_LookupAtom(fp)), ARG2);
}

Atom Yap_TemporaryFile(const char *prefix, int *fd) {
#if HAVE_MKSTEMP
  char *tmp = malloc(PATH_MAX);
  int n;
  int f;
  if (tmp == NULL)
    return NIL;
  strncpy(tmp, prefix, PATH_MAX - 1);
  n = strlen(tmp);
  if (n >= 6 && tmp[n - 1] == 'X' && tmp[n - 2] == 'X' && tmp[n - 3] == 'X' &&
      tmp[n - 4] == 'X' && tmp[n - 5] == 'X' && tmp[n - 6] == 'X')
    f = mkstemp(tmp);
  else {
    strncat(tmp, "XXXXXX", PATH_MAX - 1);
    f = mkstemp(tmp);
  }
  if (fd)
    *fd = f;
  return Yap_LookupAtom(tmp);
#else
  return AtomNil;
#endif
}

/** @pred make_directory(+ _Dir_)

Create a directory  _Dir_. The name of the directory must be an atom.

*/
static Int make_directory(USES_REGS1) {
  const char *fd = AtomName(AtomOfTerm(ARG1));
#if defined(__MINGW32__) || _MSC_VER
  if (_mkdir(fd) == -1) {
#else
  if (mkdir(fd, 0777) == -1) {
#endif
    /* return an error number */
    return false; // errno?
  }
  return true;
}

static Int p_rmdir(USES_REGS1) {
  const char *fd = AtomName(AtomOfTerm(ARG1));
#if defined(__MINGW32__) || _MSC_VER
  if (_rmdir(fd) == -1) {
#else
  if (rmdir(fd) == -1) {
#endif
    /* return an error number */
    return (Yap_unify(ARG2, MkIntTerm(errno)));
  }
  return true;
}

static bool initSysPath(Term tlib, Term tcommons, bool dir_done,
                        bool commons_done) {
  CACHE_REGS
  int len;

#if __WINDOWS__
  {
    char *dir;
    if ((dir = Yap_RegistryGetString("library")) && is_directory(dir)) {
      if (!Yap_unify(tlib, MkAtomTerm(Yap_LookupAtom(dir))))
        return FALSE;
    }
    dir_done = true;
    if ((dir = Yap_RegistryGetString("prolog_commons")) && is_directory(dir)) {
      if (!Yap_unify(tcommons, MkAtomTerm(Yap_LookupAtom(dir))))
        return FALSE;
    }
    commons_done = true;
  }
  if (dir_done && commons_done)
    return TRUE;
#endif
  strncpy(LOCAL_FileNameBuf, YAP_SHAREDIR, YAP_FILENAME_MAX);
  strncat(LOCAL_FileNameBuf, "/", YAP_FILENAME_MAX);
  len = strlen(LOCAL_FileNameBuf);
  if (!dir_done) {
    strncat(LOCAL_FileNameBuf, "Yap", YAP_FILENAME_MAX);
    if (is_directory(LOCAL_FileNameBuf)) {
      if (!Yap_unify(tlib, MkAtomTerm(Yap_LookupAtom(LOCAL_FileNameBuf))))
        return FALSE;
      dir_done = true;
    }
  }
  if (!commons_done) {
    LOCAL_FileNameBuf[len] = '\0';
    strncat(LOCAL_FileNameBuf, "PrologCommons", YAP_FILENAME_MAX);
    if (is_directory(LOCAL_FileNameBuf)) {
      if (!Yap_unify(tcommons, MkAtomTerm(Yap_LookupAtom(LOCAL_FileNameBuf))))
        return FALSE;
    }
    commons_done = true;
  }
  if (dir_done && commons_done)
    return TRUE;

#if __WINDOWS__
  {
    size_t buflen;
    char *pt;
    /* couldn't find it where it was supposed to be,
   let's try using the executable */
    if (!GetModuleFileName(NULL, LOCAL_FileNameBuf, YAP_FILENAME_MAX)) {
      Yap_WinError("could not find executable name");
      /* do nothing */
      return FALSE;
    }
    buflen = strlen(LOCAL_FileNameBuf);
    pt = LOCAL_FileNameBuf + buflen;
    while (*--pt != '\\') {
      /* skip executable */
      if (pt == LOCAL_FileNameBuf) {
        FileError(SYSTEM_ERROR_OPERATING_SYSTEM, TermNil,
                  "could not find executable name");
        /* do nothing */
        return FALSE;
      }
    }
    while (*--pt != '\\') {
      /* skip parent directory "bin\\" */
      if (pt == LOCAL_FileNameBuf) {
        FileError(SYSTEM_ERROR_OPERATING_SYSTEM, TermNil,
                  "could not find executable name");
        /* do nothing */
        return FALSE;
      }
    }
    /* now, this is a possible location for the ROOT_DIR, let's look for a share
     * directory here */
    pt[1] = '\0';
    /* grosse */
    strncat(LOCAL_FileNameBuf, "lib\\Yap", YAP_FILENAME_MAX);
    libdir = Yap_AllocCodeSpace(strlen(LOCAL_FileNameBuf) + 1);
    strncpy(libdir, LOCAL_FileNameBuf, strlen(LOCAL_FileNameBuf) + 1);
    pt[1] = '\0';
    strncat(LOCAL_FileNameBuf, "share", YAP_FILENAME_MAX);
  }
  strncat(LOCAL_FileNameBuf, "\\", YAP_FILENAME_MAX);
  len = strlen(LOCAL_FileNameBuf);
  strncat(LOCAL_FileNameBuf, "Yap", YAP_FILENAME_MAX);
  if (!dir_done && is_directory(LOCAL_FileNameBuf)) {
    if (!Yap_unify(tlib, MkAtomTerm(Yap_LookupAtom(LOCAL_FileNameBuf))))
      return FALSE;
  }
  dir_done = true;
  LOCAL_FileNameBuf[len] = '\0';
  strncat(LOCAL_FileNameBuf, "PrologCommons", YAP_FILENAME_MAX);
  if (!commons_done && is_directory(LOCAL_FileNameBuf)) {
    if (!Yap_unify(tcommons, MkAtomTerm(Yap_LookupAtom(LOCAL_FileNameBuf))))
      return FALSE;
  }
  commons_done = true;
#endif
  return dir_done && commons_done;
}

static Int libraries_directories(USES_REGS1) {
  return initSysPath(ARG1, ARG2, false, false);
}

static Int system_library(USES_REGS1) {
#if __ANDROID__
  static Term dir = 0;
  Term t;
  if (IsVarTerm(t=Deref(ARG1))) {
    if (dir == 0)
      return false;
    return Yap_unify(dir, ARG1);
  }
  if (!IsAtomTerm(t))
    return false;
  dir = t;
  return true;
#else
  return initSysPath(ARG1, MkVarTerm(), false, true);
#endif
}

static Int commons_library(USES_REGS1) {
  return initSysPath(MkVarTerm(), ARG1, true, false);
}

static Int p_dir_sp(USES_REGS1) {
#if ATARI || _MSC_VER || defined(__MINGW32__)
  Term t = MkIntTerm('\\');
  Term t2 = MkIntTerm('/');
#else
  Term t = MkIntTerm('/');
  Term t2 = MkIntTerm('/');
#endif

  return Yap_unify_constant(ARG1, t) || Yap_unify_constant(ARG1, t2);
}

size_t Yap_InitPageSize(void) {
#ifdef _WIN32
  SYSTEM_INFO si;
  GetSystemInfo(&si);
  return si.dwPageSize;
#elif HAVE_UNISTD_H
#if defined(__FreeBSD__) || defined(__DragonFly__)
  return getpagesize();
#elif defined(_AIX)
  return sysconf(_SC_PAGE_SIZE);
#elif !defined(_SC_PAGESIZE)
  return getpagesize();
#else
  return sysconf(_SC_PAGESIZE);
#endif
#else
  bla bla
#endif
}

/* TrueFileName -> Finds the true name of a file */

#ifdef __MINGW32__
#include <ctype.h>
#endif

static int volume_header(char *file) {
#if _MSC_VER || defined(__MINGW32__)
  char *ch = file;
  int c;

  while ((c = ch[0]) != '\0') {
    if (isalnum(c))
      ch++;
    else
      return (c == ':');
  }
#endif
  return (FALSE);
}

int Yap_volume_header(char *file) { return volume_header(file); }

const char *Yap_getcwd(const char *cwd, size_t cwdlen) {
#if _WIN32 || defined(__MINGW32__)
  if (GetCurrentDirectory(cwdlen, (char *)cwd) == 0) {
    Yap_WinError("GetCurrentDirectory failed");
    return NULL;
  }
  return (char *)cwd;
#endif
  return getcwd((char *)cwd, cwdlen);
}

static Int working_directory(USES_REGS1) {
  char dir[YAP_FILENAME_MAX + 1];
  Term t1 = Deref(ARG1), t2;

  if (!IsVarTerm(t1) && !IsAtomTerm(t1)) {
    Yap_Error(TYPE_ERROR_ATOM, t1, "working_directory");
  }
  if (!Yap_unify(t1,
                 MkAtomTerm(Yap_LookupAtom(Yap_getcwd(dir, YAP_FILENAME_MAX)))))
    return false;
  t2 = Deref(ARG2);
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t2, "working_directory");
  }
  if (!IsAtomTerm(t2)) {
    Yap_Error(TYPE_ERROR_ATOM, t2, "working_directory");
  }
  if (t2 == TermEmptyAtom || t2 == TermDot)
    return true;
  return ChDir(RepAtom(AtomOfTerm(t2))->StrOfAE);
}

/** Yap_findFile(): tries to locate a file, no expansion should be performed/
   *
   *
   * @param isource the proper file
   * @param idef the default name fothe file, ie, startup.yss
   * @param root the prefix
   * @param result the output
   * @param access verify whether the file has access permission
   * @param ftype saved state, object, saved file, prolog file
   * @param expand_root expand $ ~, etc
   * @param in_lib library file
   *
   * @return
   */
const char *Yap_findFile(const char *isource, const char *idef,
                         const char *iroot, char *result, bool access,
YAP_file_type_t ftype, bool expand_root, bool in_lib) {



  char *save_buffer = NULL;
  const char *root, *source = isource;
  int rc = FAIL_RESTORE;
  int try = 0;
  bool  abspath = false;
//__android_log_print(ANDROID_LOG_ERROR,  "YAPDroid " __FUNCTION__,
    // "try=%d %s %s", try, isource, iroot) ; }
  while (rc == FAIL_RESTORE) {
    // means we failed this iteration
    bool done = false;
    // { CACHE_REGS
    switch (try ++) {
    case 0: // path or file name is given;
      root = iroot;
      if (idef || isource) {
        source = (isource ? isource : idef);
      }
      if (source) {
	if (source[0] == '/') {
	  abspath = true;
	}
     }
      if (!abspath && !root && ftype == YAP_BOOT_PL) {
	root = YAP_PL_SRCDIR;
      }
      break;
    case 1: // library directory is given in command line
      if (in_lib && ftype == YAP_SAVED_STATE) {
        root = iroot;
        source = (isource ? isource : idef);
      } else {
        done = true;
      }
      break;
    case 2: // use environment variable YAPLIBDIR
#if HAVE_GETENV
      if (in_lib) {
        if (ftype == YAP_SAVED_STATE || ftype == YAP_OBJ) {
          root = getenv("YAPLIBDIR");
        } else if (ftype == YAP_BOOT_PL) {
          root = getenv("YAPSHAREDIR" "/pl");
          if (root == NULL) {
            continue;
          } else {
            save_buffer = getFileNameBuffer();
             strncpy(save_buffer, root, YAP_FILENAME_MAX);
            strncat(save_buffer, "/pl", YAP_FILENAME_MAX);
          }
        }
        source = (isource ? isource : idef);
      } else
#endif
        done = true;
      break;
    case 3: // use compilation variable YAPLIBDIR
      if (in_lib) {
        source = (isource ? isource : idef);
        if (ftype == YAP_PL) {
          root = YAP_SHAREDIR;
        } else if (ftype == YAP_BOOT_PL) {
          root = YAP_SHAREDIR "/pl";
        } else {
          root = YAP_LIBDIR;
        }
      } else
        done = true;
      break;

    case 4: // WIN stuff: registry
#if __WINDOWS__
      if (in_lib) {
        source = (ftype == YAP_PL || ftype == YAP_QLY ? "library" : "startup");
        source = Yap_RegistryGetString(source);
        root = NULL;
      } else
#endif
        done = true;
      break;

    case 5: // search from the binary
#ifndef __ANDROID__
    {
      done = true;
    } break;
#endif
      {
        const char *pt = Yap_FindExecutable();

        if (pt) {
          if (ftype == YAP_BOOT_PL) {
#if __ANDROID__
            root = "../../../files/Yap/pl";
#else
            root = "../../share/Yap/pl";
    #endif
          } else {
            root = (ftype == YAP_SAVED_STATE || ftype == YAP_OBJ
                        ? "../../lib/Yap"
                        : "../../share/Yap");
          }
          if (root == iroot) {
            done = true;
            continue;
          }
          if (!save_buffer)
            save_buffer = getFileNameBuffer();
          if (Yap_findFile(source, NULL, root, save_buffer, access, ftype,
                           expand_root, in_lib))
            root = save_buffer;
          else
            done = true;
        } else {
          done = true;
        }
        source = (isource ? isource : idef);
      }
      break;
    case 6: // default, try current directory
      if (!isource && ftype == YAP_SAVED_STATE)
        source = idef;
      root = NULL;
      break;
    default:
      if (save_buffer)
        freeFileNameBuffer(save_buffer);

      return false;
    }

    if (done)
      continue;
    //    { CACHE_REGS __android_log_print(ANDROID_LOG_ERROR,  __FUNCTION__,
    //    "root= %s %s ", root, source) ; }
    const char *work = PlExpandVars(source, root, result);

    if (save_buffer)
      freeFileNameBuffer(save_buffer);

  // expand names in case you have
    // to add a prefix
    if (!access || Yap_Exists(work)) {
      return work; // done
    } else if (abspath)
      return NULL;
  }
  return NULL;
}

static Int true_file_name(USES_REGS1) {
  Term t = Deref(ARG1);
  const char *s;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "argument to true_file_name unbound");
    return FALSE;
  }
  if (IsAtomTerm(t)) {
    s = RepAtom(AtomOfTerm(t))->StrOfAE;
  } else if (IsStringTerm(t)) {
    s = StringOfTerm(t);
  } else {
    Yap_Error(TYPE_ERROR_ATOM, t, "argument to true_file_name");
    return FALSE;
  }
  if (!(s = Yap_AbsoluteFile(s, LOCAL_FileNameBuf, true)))
    return false;
  bool rc = Yap_unify(ARG2, MkAtomTerm(Yap_LookupAtom(s)));
  freeBuffer(s);
  return rc;
}

static Int p_expand_file_name(USES_REGS1) {
  Term t = Deref(ARG1);
  const char *text, *text2;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "argument to true_file_name unbound");
    return FALSE;
  }
  text = Yap_TextTermToText(t, NULL, 0, LOCAL_encoding);
  if (!text)
    return false;
  if (!(text2 = PlExpandVars(text, NULL, NULL)))
    return false;
  freeBuffer(text);
  bool rc = Yap_unify(ARG2, Yap_MkTextTerm(text2, LOCAL_encoding, t));
  freeBuffer(text2);
  return rc;
}

static Int true_file_name3(USES_REGS1) {
  Term t = Deref(ARG1), t2 = Deref(ARG2);
  char *root = NULL;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "argument to true_file_name unbound");
    return FALSE;
  }
  if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM, t, "argument to true_file_name");
    return FALSE;
  }
  if (!IsVarTerm(t2)) {
    if (!IsAtomTerm(t)) {
      Yap_Error(TYPE_ERROR_ATOM, t2, "argument to true_file_name");
      return FALSE;
    }
    root = RepAtom(AtomOfTerm(t2))->StrOfAE;
  }
  if (!Yap_findFile(RepAtom(AtomOfTerm(t))->StrOfAE, NULL, root,
                    LOCAL_FileNameBuf, false, YAP_PL, false, false))
    return FALSE;
  return Yap_unify(ARG3, MkAtomTerm(Yap_LookupAtom(LOCAL_FileNameBuf)));
}

/* Executes $SHELL under Prolog */
/** @pred  sh


      Creates a new shell interaction.


  */
static Int p_sh(USES_REGS1) { /* sh				 */
#ifdef HAVE_SYSTEM
  char *shell;
  shell = (char *)getenv("SHELL");
  if (shell == NULL)
    shell = "/bin/sh";
  if (system(shell) < 0) {
#if HAVE_STRERROR
    Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, TermNil, "%s in sh/0",
              strerror(errno));
#else
    Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, TermNil, "in sh/0");
#endif
    return FALSE;
  }
  return TRUE;
#else
#ifdef MSH
  register char *shell;
  shell = "msh -i";
  system(shell);
  return (TRUE);
#else
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
            "sh not available in this configuration");
  return (FALSE);
#endif /* MSH */
#endif
}

/** shell(+Command:text, -Status:integer) is det.

      Run an external command and wait for its completion.
  */
static Int p_shell(USES_REGS1) { /* '$shell'(+SystCommand) */
  const char *cmd;
  Term t1 = Deref(ARG1);
  if (IsAtomTerm(t1))
    cmd = RepAtom(AtomOfTerm(t1))->StrOfAE;
  else if (IsStringTerm(t1))
    cmd = StringOfTerm(t1);
  else
    return FALSE;
#if _MSC_VER || defined(__MINGW32__)
  {
    int rval = system(cmd);

    return rval == 0;
  }

  return true;
#else
#if HAVE_SYSTEM
  char *shell;
  register int bourne = FALSE;

  shell = (char *)getenv("SHELL");
  if (!strcmp(shell, "/bin/sh"))
    bourne = TRUE;
  if (shell == NIL)
    bourne = TRUE;
  /* Yap_CloseStreams(TRUE); */
  if (bourne)
    return system(cmd) == 0;
  else {
    int status = -1;
    int child = fork();

    if (child == 0) { /* let the children go */
      if (!execl(shell, shell, "-c", cmd, NULL)) {
        exit(-1);
      }
      exit(TRUE);
    }
    { /* put the father on wait */
      int result = child < 0 ||
                   /* vsc:I am not sure this is used, Stevens say wait returns
                an integer.
                #if NO_UNION_WAIT
             */
                   wait((&status)) != child ||
                   /*
               #else
               wait ((union wait *) (&status)) != child ||
               #endif
             */
                   status == 0;
      return result;
    }
  }
#else /* HAVE_SYSTEM */
#ifdef MSH
  register char *shell;
  shell = "msh -i";
  /* Yap_CloseStreams(); */
  system(shell);
  return TRUE;
#else
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
            "shell not available in this configuration");
  return FALSE;
#endif
#endif /* HAVE_SYSTEM */
#endif /* _MSC_VER */
}

/** system(+Command:text).

      Run an external command.
  */

static Int p_system(USES_REGS1) { /* '$system'(+SystCommand)	       */
  const char *cmd;
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "argument to system/1 unbound");
    return FALSE;
  } else if (IsAtomTerm(t1)) {
    cmd = RepAtom(AtomOfTerm(t1))->StrOfAE;
  } else if (IsStringTerm(t1)) {
    cmd = StringOfTerm(t1);
  } else {
    if (!Yap_GetName(LOCAL_FileNameBuf, YAP_FILENAME_MAX, t1)) {
      Yap_Error(TYPE_ERROR_ATOM, t1, "argument to system/1");
      return false;
    }
    cmd = LOCAL_FileNameBuf;
  }
/* Yap_CloseStreams(TRUE); */
#if _MSC_VER || defined(__MINGW32__)

  {
    STARTUPINFO si;
    PROCESS_INFORMATION pi;

    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    ZeroMemory(&pi, sizeof(pi));

    // Start the child process.
    if (!CreateProcess(NULL,       // No module name (use command line)
                       (LPSTR)cmd, // Command line
                       NULL,       // Process handle not inheritable
                       NULL,       // Thread handle not inheritable
                       FALSE,      // Set handle inheritance to FALSE
                       0,          // No creation flags
                       NULL,       // Use parent's environment block
                       NULL,       // Use parent's starting directory
                       &si,        // Pointer to STARTUPINFO structure
                       &pi)        // Pointer to PROCESS_INFORMATION structure
        ) {
      Yap_Error(SYSTEM_ERROR_INTERNAL, ARG1, "CreateProcess failed (%d).\n",
                GetLastError());
      return FALSE;
    }
    // Wait until child process exits.
    WaitForSingleObject(pi.hProcess, INFINITE);

    // Close process and thread handles.
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);

    return TRUE;
  }

  return FALSE;
#elif HAVE_SYSTEM
#if _MSC_VER
  _flushall();
#endif
  if (system(cmd)) {
#if HAVE_STRERROR
    Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, t1, "%s in system(%s)",
              strerror(errno), cmd);
#else
    Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, t1, "in system(%s)", cmd);
#endif
    return FALSE;
  }
  return TRUE;
#else
#ifdef MSH
  register char *shell;
  shell = "msh -i";
  /* Yap_CloseStreams(); */
  system(shell);
  return (TRUE);
#undef command
#else
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "sh not available in this machine");
  return (FALSE);
#endif
#endif /* HAVE_SYSTEM */
}

static Int p_mv(USES_REGS1) { /* rename(+OldName,+NewName)   */
#if HAVE_LINK
  int r;
  char *oldname, *newname;
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "first argument to rename/2 unbound");
  } else if (!IsAtomTerm(t1)) {
    Yap_Error(TYPE_ERROR_ATOM, t1, "first argument to rename/2 not atom");
  }
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t2, "second argument to rename/2 unbound");
  } else if (!IsAtomTerm(t2)) {
    Yap_Error(TYPE_ERROR_ATOM, t2, "second argument to rename/2 not atom");
  } else {
    oldname = (RepAtom(AtomOfTerm(t1)))->StrOfAE;
    newname = (RepAtom(AtomOfTerm(t2)))->StrOfAE;
    if ((r = link(oldname, newname)) == 0 && (r = unlink(oldname)) != 0)
      unlink(newname);
    if (r != 0) {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, t2, "%s in rename(%s,%s)",
                strerror(errno), oldname, newname);
#else
      Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, t2, "in rename(%s,%s)", oldname,
                newname);
#endif
      return false;
    }
    return true;
  }
#else
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
            "rename/2 not available in this machine");
#endif
  return false;
}

#ifdef MAC

void Yap_SetTextFile(name) char *name;
{
#ifdef MACC
  SetFileType(name, 'TEXT');
  SetFileSignature(name, 'EDIT');
#else
  FInfo f;
  FInfo *p = &f;
  GetFInfo(name, 0, p);
  p->fdType = 'TEXT';
#ifdef MPW
  if (mpwshell)
    p->fdCreator = 'MPS\0';
#endif
#ifndef LIGHT
  else
    p->fdCreator = 'EDIT';
#endif
  SetFInfo(name, 0, p);
#endif
}

#endif

/* return YAP's environment */
static Int p_getenv(USES_REGS1) {
#if HAVE_GETENV
  Term t1 = Deref(ARG1), to;
  char *s, *so;

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "first arg of getenv/2");
    return (FALSE);
  } else if (!IsAtomTerm(t1)) {
    Yap_Error(TYPE_ERROR_ATOM, t1, "first arg of getenv/2");
    return (FALSE);
  } else
    s = RepAtom(AtomOfTerm(t1))->StrOfAE;
  if ((so = getenv(s)) == NULL)
    return (FALSE);
  to = MkAtomTerm(Yap_LookupAtom(so));
  return (Yap_unify_constant(ARG2, to));
#else
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
            "getenv not available in this configuration");
  return (FALSE);
#endif
}

/* set a variable in YAP's environment */
static Int p_putenv(USES_REGS1) {
#if HAVE_PUTENV
  Term t1 = Deref(ARG1), t2 = Deref(ARG2);
  char *s = "", *s2 = "", *p0, *p;

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "first arg to putenv/2");
    return (FALSE);
  } else if (!IsAtomTerm(t1)) {
    Yap_Error(TYPE_ERROR_ATOM, t1, "first arg to putenv/2");
    return (FALSE);
  } else
    s = RepAtom(AtomOfTerm(t1))->StrOfAE;
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "second arg to putenv/2");
    return (FALSE);
  } else if (!IsAtomTerm(t2)) {
    Yap_Error(TYPE_ERROR_ATOM, t2, "second arg to putenv/2");
    return (FALSE);
  } else
    s2 = RepAtom(AtomOfTerm(t2))->StrOfAE;
  while (!(p0 = p = Yap_AllocAtomSpace(strlen(s) + strlen(s2) + 3))) {
    if (!Yap_growheap(FALSE, MinHeapGap, NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, LOCAL_ErrorMessage);
      return FALSE;
    }
  }
  while ((*p++ = *s++) != '\0')
    ;
  p[-1] = '=';
  while ((*p++ = *s2++) != '\0')
    ;
  if (putenv(p0) == 0)
    return TRUE;
#if HAVE_STRERROR
  Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, TermNil, "in putenv(%s)",
            strerror(errno), p0);
#else
  Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, TermNil, "in putenv(%s)", p0);
#endif
  return FALSE;
#else
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
            "putenv not available in this configuration");
  return FALSE;
#endif
}

static Int p_host_type(USES_REGS1) {
  Term out = MkAtomTerm(Yap_LookupAtom(HOST_ALIAS));
  return (Yap_unify(out, ARG1));
}

static Int p_yap_home(USES_REGS1) {
  Term out = MkAtomTerm(Yap_LookupAtom(YAP_ROOTDIR));
  return (Yap_unify(out, ARG1));
}

static Int p_yap_paths(USES_REGS1) {
  Term out1, out2, out3;
  const char *env_destdir = getenv("DESTDIR");
  char destdir[YAP_FILENAME_MAX + 1];

  if (env_destdir) {
    strncat(destdir, env_destdir, YAP_FILENAME_MAX);
    strncat(destdir, "/" YAP_LIBDIR, YAP_FILENAME_MAX);
    out1 = MkAtomTerm(Yap_LookupAtom(destdir));
  } else {
    out1 = MkAtomTerm(Yap_LookupAtom(YAP_LIBDIR));
  }
  if (env_destdir) {
    strncat(destdir, env_destdir, YAP_FILENAME_MAX);
    strncat(destdir, "/" YAP_SHAREDIR, YAP_FILENAME_MAX);
    out2 = MkAtomTerm(Yap_LookupAtom(destdir));
  } else {
    out2 = MkAtomTerm(Yap_LookupAtom(YAP_SHAREDIR));
  }
  if (env_destdir) {
    strncat(destdir, env_destdir, YAP_FILENAME_MAX);
    strncat(destdir, "/" YAP_BINDIR, YAP_FILENAME_MAX);
    out3 = MkAtomTerm(Yap_LookupAtom(destdir));
  } else {
    out3 = MkAtomTerm(Yap_LookupAtom(YAP_BINDIR));
  }
  return (Yap_unify(out1, ARG1) && Yap_unify(out2, ARG2) &&
          Yap_unify(out3, ARG3));
}

static Int p_log_event(USES_REGS1) {
  Term in = Deref(ARG1);
  Atom at;

  if (IsVarTerm(in))
    return FALSE;
  if (!IsAtomTerm(in))
    return FALSE;
  at = AtomOfTerm(in);
#if DEBUG
  if (IsWideAtom(at))
    fprintf(stderr, "LOG %S\n", RepAtom(at)->WStrOfAE);
  else if (IsBlob(at))
    return FALSE;
  else
    fprintf(stderr, "LOG %s\n", RepAtom(at)->StrOfAE);
#endif
  if (IsWideAtom(at) || IsBlob(at))
    return FALSE;
  LOG(" %s ", RepAtom(at)->StrOfAE);
  return TRUE;
}

static Int p_env_separator(USES_REGS1) {
#if defined(_WIN32)
  return Yap_unify(MkIntegerTerm(';'), ARG1);
#else
  return Yap_unify(MkIntegerTerm(':'), ARG1);
#endif
}

/*
   * This is responsable for the initialization of all machine dependant
   * predicates
   */
void Yap_InitSysbits(int wid) {
  CACHE_REGS
#if __simplescalar__
  {
    char *pwd = getenv("PWD");
    strncpy(GLOBAL_pwd, pwd, YAP_FILENAME_MAX);
  }
#endif
  Yap_InitWTime();
  Yap_InitRandom();
  /* let the caller control signals as it sees fit */
  Yap_InitOSSignals(worker_id);
}

static Int p_unix(USES_REGS1) {
#ifdef unix
  return TRUE;
#else
#ifdef __unix__
  return TRUE;
#else
#ifdef __APPLE__
  return TRUE;
#else
  return FALSE;
#endif
#endif
#endif
}

static Int p_win32(USES_REGS1) {
#ifdef _WIN32
  return TRUE;
#else
#ifdef __CYGWIN__
  return TRUE;
#else
  return FALSE;
#endif
#endif
}

static Int p_ld_path(USES_REGS1) {
  return Yap_unify(ARG1, MkAtomTerm(Yap_LookupAtom(YAP_LIBDIR)));
}

static Int p_address_bits(USES_REGS1) {
#if SIZEOF_INT_P == 4
  return Yap_unify(ARG1, MkIntTerm(32));
#else
  return Yap_unify(ARG1, MkIntTerm(64));
#endif
}

#ifdef _WIN32

/* This code is from SWI-Prolog by Jan Wielemaker */

#define wstreq(s, q) (wcscmp((s), (q)) == 0)

static HKEY reg_open_key(const wchar_t *which, int create) {
  HKEY key = HKEY_CURRENT_USER;
  DWORD disp;
  LONG rval;

  while (*which) {
    wchar_t buf[256];
    wchar_t *s;
    HKEY tmp;

    for (s = buf; *which && !(*which == '/' || *which == '\\');)
      *s++ = *which++;
    *s = '\0';
    if (*which)
      which++;

    if (wstreq(buf, L"HKEY_CLASSES_ROOT")) {
      key = HKEY_CLASSES_ROOT;
      continue;
    } else if (wstreq(buf, L"HKEY_CURRENT_USER")) {
      key = HKEY_CURRENT_USER;
      continue;
    } else if (wstreq(buf, L"HKEY_LOCAL_MACHINE")) {
      key = HKEY_LOCAL_MACHINE;
      continue;
    } else if (wstreq(buf, L"HKEY_USERS")) {
      key = HKEY_USERS;
      continue;
    }

    if (RegOpenKeyExW(key, buf, 0L, KEY_READ, &tmp) == ERROR_SUCCESS) {
      RegCloseKey(key);
      key = tmp;
      continue;
    }

    if (!create)
      return NULL;

    rval =
        RegCreateKeyExW(key, buf, 0, L"", 0, KEY_ALL_ACCESS, NULL, &tmp, &disp);
    RegCloseKey(key);
    if (rval == ERROR_SUCCESS)
      key = tmp;
    else
      return NULL;
  }

  return key;
}

#define MAXREGSTRLEN 1024

static void recover_space(wchar_t *k, Atom At) {
  if (At->WStrOfAE != k)
    Yap_FreeCodeSpace((char *)k);
}

static wchar_t *WideStringFromAtom(Atom KeyAt USES_REGS) {
  if (IsWideAtom(KeyAt)) {
    return KeyAt->WStrOfAE;
  } else {
    int len = strlen(KeyAt->StrOfAE);
    int sz = sizeof(wchar_t) * (len + 1);
    char *chp = KeyAt->StrOfAE;
    wchar_t *kptr, *k;

    k = (wchar_t *)Yap_AllocCodeSpace(sz);
    while (k == NULL) {
      if (!Yap_growheap(false, sz, NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, MkIntegerTerm(sz),
                  "generating key in win_registry_get_value/3");
        return false;
      }
      k = (wchar_t *)Yap_AllocCodeSpace(sz);
    }
    kptr = k;
    while ((*kptr++ = *chp++))
      ;
    return k;
  }
}

static Int p_win_registry_get_value(USES_REGS1) {
  DWORD type;
  BYTE data[MAXREGSTRLEN];
  DWORD len = sizeof(data);
  wchar_t *k, *name;
  HKEY key;
  Term Key = Deref(ARG1);
  Term Name = Deref(ARG2);
  Atom KeyAt, NameAt;

  if (IsVarTerm(Key)) {
    Yap_Error(INSTANTIATION_ERROR, Key,
              "argument to win_registry_get_value unbound");
    return FALSE;
  }
  if (!IsAtomTerm(Key)) {
    Yap_Error(TYPE_ERROR_ATOM, Key, "argument to win_registry_get_value");
    return FALSE;
  }
  KeyAt = AtomOfTerm(Key);
  if (IsVarTerm(Name)) {
    Yap_Error(INSTANTIATION_ERROR, Key,
              "argument to win_registry_get_value unbound");
    return FALSE;
  }
  if (!IsAtomTerm(Name)) {
    Yap_Error(TYPE_ERROR_ATOM, Key, "argument to win_registry_get_value");
    return FALSE;
  }
  NameAt = AtomOfTerm(Name);

  k = WideStringFromAtom(KeyAt PASS_REGS);
  if (!(key = reg_open_key(k, FALSE))) {
    Yap_Error(EXISTENCE_ERROR_KEY, Key, "argument to win_registry_get_value");
    recover_space(k, KeyAt);
    return FALSE;
  }
  name = WideStringFromAtom(NameAt PASS_REGS);

  if (RegQueryValueExW(key, name, NULL, &type, data, &len) == ERROR_SUCCESS) {
    RegCloseKey(key);
    switch (type) {
    case REG_SZ:
      recover_space(k, KeyAt);
      recover_space(name, NameAt);
      ((wchar_t *)data)[len] = '\0';
      return Yap_unify(MkAtomTerm(Yap_LookupMaybeWideAtom((wchar_t *)data)),
                       ARG3);
    case REG_DWORD:
      recover_space(k, KeyAt);
      recover_space(name, NameAt);
      {
        DWORD *d = (DWORD *)data;
        return Yap_unify(MkIntegerTerm((Int)d[0]), ARG3);
      }
    default:
      recover_space(k, KeyAt);
      recover_space(name, NameAt);
      return FALSE;
    }
  }
  recover_space(k, KeyAt);
  recover_space(name, NameAt);
  return FALSE;
}

char *Yap_RegistryGetString(char *name) {
  DWORD type;
  BYTE data[MAXREGSTRLEN];
  DWORD len = sizeof(data);
  HKEY key;
  char *ptr;
  int i;

#if SIZEOF_INT_P == 8
  if (!(key =
            reg_open_key(L"HKEY_LOCAL_MACHINE/SOFTWARE/YAP/Prolog64", FALSE))) {
    return NULL;
  }
#else
  if (!(key = reg_open_key(L"HKEY_LOCAL_MACHINE/SOFTWARE/YAP/Prolog", FALSE))) {
    return NULL;
  }
#endif
  if (RegQueryValueEx(key, name, NULL, &type, data, &len) == ERROR_SUCCESS) {
    RegCloseKey(key);
    switch (type) {
    case REG_SZ:
      ptr = malloc(len + 2);
      if (!ptr)
        return NULL;
      for (i = 0; i <= len; i++)
        ptr[i] = data[i];
      ptr[len + 1] = '\0';
      return ptr;
    default:
      return NULL;
    }
  }
  return NULL;
}

#endif

void Yap_InitSysPreds(void) {
  Yap_InitCPred("log_event", 1, p_log_event, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("sh", 0, p_sh, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$shell", 1, p_shell, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("system", 1, p_system,
                SafePredFlag | SyncPredFlag | UserCPredFlag);
  Yap_InitCPred("$rename", 2, p_mv, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$yap_home", 1, p_yap_home, SafePredFlag);
  Yap_InitCPred("$yap_paths", 3, p_yap_paths, SafePredFlag);
  Yap_InitCPred("$dir_separator", 1, p_dir_sp, SafePredFlag);
  Yap_InitCPred("libraries_directories", 2, libraries_directories, 0);
  Yap_InitCPred("system_library", 1, system_library, 0);
  Yap_InitCPred("commons_library", 1, commons_library, 0);
  Yap_InitCPred("$getenv", 2, p_getenv, SafePredFlag);
  Yap_InitCPred("$putenv", 2, p_putenv, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$host_type", 1, p_host_type, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$env_separator", 1, p_env_separator, SafePredFlag);
  Yap_InitCPred("$unix", 0, p_unix, SafePredFlag);
  Yap_InitCPred("$win32", 0, p_win32, SafePredFlag);
  Yap_InitCPred("$ld_path", 1, p_ld_path, SafePredFlag);
  Yap_InitCPred("$address_bits", 1, p_address_bits, SafePredFlag);
  Yap_InitCPred("$expand_file_name", 2, p_expand_file_name, SyncPredFlag);
  Yap_InitCPred("expand_file_name", 3, expand_file_name3, SyncPredFlag);
  Yap_InitCPred("expand_file_name", 2, expand_file_name, SyncPredFlag);
  Yap_InitCPred("working_directory", 2, working_directory, SyncPredFlag);
  Yap_InitCPred("prolog_to_os_filename", 2, prolog_to_os_filename,
                SyncPredFlag);
  Yap_InitCPred("absolute_file_system_path", 2, absolute_file_system_path, 0);
  Yap_InitCPred("real_path", 2, real_path, 0);
  Yap_InitCPred("true_file_name", 2, true_file_name, SyncPredFlag);
  Yap_InitCPred("true_file_name", 3, true_file_name3, SyncPredFlag);
#ifdef _WIN32
  Yap_InitCPred("win_registry_get_value", 3, p_win_registry_get_value, 0);
#endif
  Yap_InitCPred("rmdir", 2, p_rmdir, SyncPredFlag);
  Yap_InitCPred("make_directory", 1, make_directory, SyncPredFlag);
}
