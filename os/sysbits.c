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
#include "cwalk.h"

static Int p_sh(USES_REGS1);
static Int p_shell(USES_REGS1);
static Int p_system(USES_REGS1);
static Int p_mv(USES_REGS1);
static Int p_dir_sp(USES_REGS1);
static Int p_getenv(USES_REGS1);
static Int p_putenv(USES_REGS1);
#ifdef MACYAP

/* #define signal	skel_signal */
#endif /* MACYAP */

void exit(int);

#ifdef _WIN32
void Yap_WinError(char *yap_error) {
  char msg[256];
  /* Error, we could not read time */
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                NULL, GetLastError(), MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                msg, 255, NULL);
  Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, TermNil, "%s at %s", msg, yap_error);
}
#endif /* __WINDOWS__ */

#define is_valid_env_char(C)                                                   \
  (((C) >= 'a' && (C) <= 'z') || ((C) >= 'A' && (C) <= 'Z') || (C) == '_')

/// is_directory: verifies whether an expanded file name
/// points at a readable directory
bool Yap_isDirectory(const char *FileName) {

  VFS_t *vfs;
  if ((vfs = vfs_owner(FileName))) {
    return vfs->isdir(vfs, FileName);
  }
#ifdef _WIN32
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
    f = Yap_VFAlloc(f);
    if ((vfs = vfs_owner(f))) {
        return vfs->exists(vfs, f);
    }
#if _WIN32
    if (_access(f, 0) == 0)
    return true;
  if (errno == EINVAL) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "bad flags to access");
  }
  return false;
#elif HAVE_ACCESS
    if (access(f, F_OK) == 0) {
        return true;
    }
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

#define isValidEnvChar(C)                                                      \
  (((C) >= 'a' && (C) <= 'z') || ((C) >= 'A' && (C) <= 'Z') || (C) == '_')

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

#if _WIN32
#define HAVE_BASENAME 1
#define HAVE_REALPATH 1
#endif

extern char *virtual_cwd;

bool Yap_ChDir(const char *path) {
  bool rc = false;
  int lvl = push_text_stack();

    const char *qpath = Yap_AbsoluteFile(path, true);
    __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "chdir %s", path);
  VFS_t *v;
  if ((v = vfs_owner(qpath))) {
    rc = v->chdir(v, (qpath));
    pop_text_stack(lvl);
    return rc;
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
  pop_text_stack(lvl);
  return rc;
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
static bool initSysPath(Term tlib, Term tcommons, bool dir_done,
                        bool commons_done) {
  CACHE_REGS

  if (!Yap_PLDIR || !Yap_unify(tlib, MkAtomTerm(Yap_LookupAtom(Yap_PLDIR))))
    return false;

  return Yap_COMMONSDIR && Yap_unify(tcommons, MkAtomTerm(Yap_LookupAtom(Yap_COMMONSDIR)));
}

static Int libraries_directories(USES_REGS1) {
  return initSysPath(ARG1, ARG2, false, false);
}

static Int system_library(USES_REGS1) {
  return initSysPath(ARG1, MkVarTerm(), false, true);
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

const char *Yap_getcwd(char *cwd, size_t cwdlen) {
  if (GLOBAL_cwd && GLOBAL_cwd[0]) {
    strcpy(cwd, GLOBAL_cwd);
    return cwd;
  }
#if _WIN32 || defined(__MINGW32__)
  if (GetCurrentDirectory(cwdlen, (char *)cwd) == 0) {
    Yap_WinError("GetCurrentDirectory failed");
    return NULL;
  }
  return (char *)cwd;
#endif
  const char *rc = getcwd(cwd, FILENAME_MAX);
 // __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "chdir %s", rc);
return rc;
}

/** @pred  working_directory( ?_CurDir_,? _NextDir_)


Fetch the current directory at  _CurDir_. If  _NextDir_ is bound
to an atom, make its value the current working directory.

Unifies  _Old_ with an absolute path to the current working directory
and change working directory to  _New_.  Use the pattern
`working_directory(CWD, CWD)` to get the current directory.  See
also `absolute_file_name/2` and chdir/1.


*/
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
  return Yap_ChDir(RepAtom(AtomOfTerm(t2))->StrOfAE);
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
  if (system(Yap_VFAlloc(shell)) < 0) {
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
    oldname = RepAtom(AtomOfTerm(t1))->StrOfAE;
    newname = RepAtom(AtomOfTerm(t2))->StrOfAE;
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
  const char *s, *so;

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "first arg of getenv/2");
    return (FALSE);
  } else     if (IsStringTerm(t1)) {
      s = StringOfTerm(t1);
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
  const char *s = "", *s2 = "";
  char *p0, *p;

    if (IsVarTerm(t1)) {
        Yap_Error(INSTANTIATION_ERROR, t1, "first arg to putenv/2");
        return (FALSE);
    } else     if (IsStringTerm(t1)) {
            s = StringOfTerm(t1);
  } else if (!IsAtomTerm(t1)) {
    Yap_Error(TYPE_ERROR_ATOM, t1, "first arg to putenv/2");
    return (FALSE);
  } else
    s = RepAtom(AtomOfTerm(t1))->StrOfAE;
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t2, "second arg to putenv/2");
    return (FALSE);
  } else     if (IsStringTerm(t2)) {
      s2 = StringOfTerm(t2);
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
  Term out;

  out = MkAtomTerm(Yap_LookupAtom(Yap_ROOTDIR));
  return Yap_unify(out, ARG1);
}

static Int p_yap_paths(USES_REGS1) {
  Term out1, out2, out3;

  out1 = MkAtomTerm(Yap_LookupAtom(Yap_LIBDIR));
  out2 = MkAtomTerm(Yap_LookupAtom(Yap_SHAREDIR));
  out3 = MkAtomTerm(Yap_LookupAtom(Yap_BINDIR));

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
  if (IsBlob(at))
    return FALSE;
  else
    fprintf(stderr, "LOG %s\n", RepAtom(at)->StrOfAE);
#endif
  if (IsBlob(at))
    return false;
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
  return Yap_unify(ARG1, MkAtomTerm(Yap_LookupAtom(Yap_DLLDIR)));
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

static wchar_t *WideStringFromAtom(Atom KeyAt USES_REGS) {
  return Yap_AtomToWide(KeyAt);
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
  int l = push_text_stack();

  if (IsVarTerm(Key)) {
    Yap_Error(INSTANTIATION_ERROR, Key,
              "argument to win_registry_get_value unbound");
    pop_text_stack(l);
    return FALSE;
  }
  if (!IsAtomTerm(Key)) {
    Yap_Error(TYPE_ERROR_ATOM, Key, "argument to win_registry_get_value");
    pop_text_stack(l);
    return FALSE;
  }
  KeyAt = AtomOfTerm(Key);
  if (IsVarTerm(Name)) {
    Yap_Error(INSTANTIATION_ERROR, Key,
              "argument to win_registry_get_value unbound");
    pop_text_stack(l);
    return FALSE;
  }
  if (!IsAtomTerm(Name)) {
    Yap_Error(TYPE_ERROR_ATOM, Key, "argument to win_registry_get_value");
    pop_text_stack(l);
    return FALSE;
  }
  NameAt = AtomOfTerm(Name);

  k = WideStringFromAtom(KeyAt PASS_REGS);
  if (!(key = reg_open_key(k, FALSE))) {
    Yap_Error(EXISTENCE_ERROR_KEY, Key, "argument to win_registry_get_value");
    pop_text_stack(l);
    return FALSE;
  }
  name = WideStringFromAtom(NameAt PASS_REGS);

  if (RegQueryValueExW(key, name, NULL, &type, data, &len) == ERROR_SUCCESS) {
    RegCloseKey(key);
    switch (type) {
    case REG_SZ:
      ((wchar_t *)data)[len] = '\0';
      Atom at = Yap_NWCharsToAtom((wchar_t *)data, len PASS_REGS);
      pop_text_stack(l);
      return Yap_unify(MkAtomTerm(at), ARG3);
    case REG_DWORD: {
      DWORD *d = (DWORD *)data;
      pop_text_stack(l);
      return Yap_unify(MkIntegerTerm((Int)d[0]), ARG3);
    }
    default:
      pop_text_stack(l);
      return FALSE;
    }
  }
  pop_text_stack(l);
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

 
static Int p_sleep(USES_REGS1) {
  Term ts = ARG1;
#if defined(__MINGW32__) || _MSC_VER
  {
    unsigned long int secs = 0, usecs = 0, msecs;
    if (IsIntegerTerm(ts)) {
      secs = IntegerOfTerm(ts);
    } else if (IsFloatTerm(ts)) {
      double tfl = FloatOfTerm(ts);
      if (tfl > 1.0)
        secs = tfl;
      else
        usecs = tfl * 1000000;
    }
    msecs = secs * 1000 + usecs / 1000;
    Sleep(msecs);
    /* no ers possible */
    return true;
  }
#elif HAVE_NANOSLEEP
  {
    struct timespec req;
    int out;

    if (IsFloatTerm(ts)) {
      double tfl = FloatOfTerm(ts);

      req.tv_nsec = (tfl - floor(tfl)) * 1000000000;
      req.tv_sec = rint(tfl);
    } else {
      req.tv_nsec = 0;
      req.tv_sec = IntOfTerm(ts);
    }
    out = nanosleep(&req, NULL);
    return out == 0;
  }
#elif HAVE_USLEEP
  {
    useconds_t usecs;
    if (IsFloatTerm(ts)) {
      double tfl = FloatOfTerm(ts);

      usecs = rint(tfl * 1000000);
    } else {
      usecs = IntegrOfTerm(ts) * 1000000;
    }
    out = usleep(usecs);
    return;
  }
#elif HAVE_SLEEP
  {
    unsigned int secs, out;
    if (IsFloatTerm(ts)) {
      secs = rint(FloatOfTerm(ts));
    } else {
      secs = IntOfTerm(ts);
    }
    out = sleep(secs);
    return (Yap_unify(ARG2, MkIntTerm(out)));
  }
#else
  YAP_Error(SYSTEM_ERROR, 0L, "sleep not available in this configuration");
  return FALSE:
#endif
}

#ifdef HAVE_MTRACE
#include <mcheck.h>
#endif

static Int
  p_mtrace()
  {
#ifdef HAVE_MTRACE
    Term t = Deref(ARG1);
    if (t == TermTrue) mtrace();
    else if (t == TermFalse)  muntrace();
    else return false;
#endif
    return true;
  }
 
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
  Yap_InitCPred("working_directory", 2, working_directory, SyncPredFlag);
#ifdef _WIN32
  Yap_InitCPred("win_registry_get_value", 3, p_win_registry_get_value, 0);
#endif
  Yap_InitCPred("sleep", 1, p_sleep, SyncPredFlag);
  Yap_InitCPred("mtrace", 1, p_mtrace, SyncPredFlag);
}
   
