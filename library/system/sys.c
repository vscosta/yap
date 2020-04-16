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
 *
 * comments:	regular expression interpreter                           *
 *									 *
 *************************************************************************/

#include "YapInterface.h"

#include <stdlib.h>

#if __ANDROID__
#include <android/asset_manager.h>
#include <android/asset_manager_jni.h>
#include <android/log.h>
#include <jni.h>
#endif
#include "YapStreams.h"

#include "VFS.h"

#include "crypto/md5.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdio.h>
#if HAVE_TIME_H
#include <time.h>
#endif
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if HAVE_MATH_H
#include <math.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_ERRNO_H
#include <errno.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_SIGNAL_H
#include <signal.h>
#endif
#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#if HAVE_DIRENT_H
#include <dirent.h>
#endif
#if HAVE_DIRECT_H
#include <direct.h>
#endif
#if defined(__MINGW32__) || _MSC_VER
#include <io.h>
#include <process.h>
#include <windows.h>
#endif
#ifdef __MINGW32__
#ifdef HAVE_ENVIRON
#undef HAVE_ENVIRON
#endif
#endif
#if __ANDROID__
#include <android/asset_manager.h>
#include <android/asset_manager_jni.h>
#include <android/log.h>
#endif

X_API void init_sys(void);

#if defined(__MINGW32__) || _MSC_VER
static YAP_Term WinError(void) {
  char msg[256];
  /* Error, we could not read time */
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                NULL, GetLastError(), MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                msg, 256, NULL);
  return (YAP_MkAtomTerm(YAP_LookupAtom(msg)));
}
#endif

/* Return time in a structure */
static YAP_Bool sysmktime(void) {

#if defined(__MINGW32__) || _MSC_VER
  SYSTEMTIME stime, stime0;
  FILETIME ftime, ftime0;

  stime.wYear = YAP_IntOfTerm(YAP_ARG1);
  stime.wMonth = YAP_IntOfTerm(YAP_ARG2);
  stime.wDay = YAP_IntOfTerm(YAP_ARG3);
  stime.wHour = YAP_IntOfTerm(YAP_ARG4);
  stime.wMinute = YAP_IntOfTerm(YAP_ARG5);
  stime.wSecond = YAP_IntOfTerm(YAP_ARG6);
  stime.wMilliseconds = 0;
  stime0.wYear = 1970;
  stime0.wMonth = 1;
  stime0.wDay = 1;
  stime0.wHour = 12;
  stime0.wMinute = 0;
  stime0.wSecond = 0;
  stime0.wMilliseconds = 0;
  if (!SystemTimeToFileTime(&stime, &ftime)) {
    return YAP_Unify(YAP_ARG8, YAP_MkIntTerm(errno));
  }
  if (!SystemTimeToFileTime(&stime0, &ftime0)) {
    return YAP_Unify(YAP_ARG8, YAP_MkIntTerm(errno));
  }
#if __GNUC__
  {
    unsigned long long f1 = (((unsigned long long)ftime.dwHighDateTime) << 32) +
                            (unsigned long long)ftime.dwLowDateTime;
    unsigned long long f0 =
        (((unsigned long long)ftime0.dwHighDateTime) << 32) +
        (unsigned long long)ftime0.dwLowDateTime;
    return YAP_Unify(YAP_ARG7, YAP_MkIntTerm((long int)((f1 - f0) / 10000000)));
  }
#else
  return FALSE;
#endif
#else
#ifdef HAVE_MKTIME
  struct tm loc;
  time_t tim;

  loc.tm_year = YAP_IntOfTerm(YAP_ARG1) - 1900;
  loc.tm_mon = YAP_IntOfTerm(YAP_ARG2) - 1;
  loc.tm_mday = YAP_IntOfTerm(YAP_ARG3);
  loc.tm_hour = YAP_IntOfTerm(YAP_ARG4);
  loc.tm_min = YAP_IntOfTerm(YAP_ARG5);
  loc.tm_sec = YAP_IntOfTerm(YAP_ARG6);
  loc.tm_isdst = -1;

  if ((tim = mktime(&loc)) == (time_t)-1) {
    return YAP_Unify(YAP_ARG8, YAP_MkIntTerm(errno));
  }
  return YAP_Unify(YAP_ARG7, YAP_MkIntTerm(tim));
#else
  oops
#endif /* HAVE_MKTIME */
#endif /* WINDOWS */
}

/* Return time in a structure */
static YAP_Bool datime(void) {
  YAP_Term tf, out[6];
#if defined(__MINGW32__) || _MSC_VER
  SYSTEMTIME stime;
  GetLocalTime(&stime);
  out[0] = YAP_MkIntTerm(stime.wYear);
  out[1] = YAP_MkIntTerm(stime.wMonth);
  out[2] = YAP_MkIntTerm(stime.wDay);
  out[3] = YAP_MkIntTerm(stime.wHour);
  out[4] = YAP_MkIntTerm(stime.wMinute);
  out[5] = YAP_MkIntTerm(stime.wSecond);
#elif HAVE_TIME
  time_t tp;

  if ((tp = time(NULL)) == -1) {
    return (YAP_Unify(YAP_ARG2, YAP_MkIntTerm(errno)));
  }
#ifdef HAVE_LOCALTIME
  {
    struct tm *loc = localtime(&tp);
    if (loc == NULL) {
      return (YAP_Unify(YAP_ARG2, YAP_MkIntTerm(errno)));
    }
    out[0] = YAP_MkIntTerm(1900 + loc->tm_year);
    out[1] = YAP_MkIntTerm(1 + loc->tm_mon);
    out[2] = YAP_MkIntTerm(loc->tm_mday);
    out[3] = YAP_MkIntTerm(loc->tm_hour);
    out[4] = YAP_MkIntTerm(loc->tm_min);
    out[5] = YAP_MkIntTerm(loc->tm_sec);
  }
#else
  oops
#endif /* HAVE_LOCALTIME */
#else
  oops
#endif /* HAVE_TIME */
  tf = YAP_MkApplTerm(YAP_MkFunctor(YAP_LookupAtom("datime"), 6), 6, out);
  return YAP_Unify(YAP_ARG1, tf);
}

#define BUF_SIZE 1024


static YAP_Bool p_unlink(void) {
  char *fd = (char *)YAP_AtomName(YAP_AtomOfTerm(YAP_ARG1));
#if defined(__MINGW32__) || _MSC_VER
  if (_unlink(fd) == -1)
#else
  if (unlink(fd) == -1)
#endif
  {
    /* return an error number */
    return (YAP_Unify(YAP_ARG2, YAP_MkIntTerm(errno)));
  }
  return (TRUE);
}

static YAP_Bool p_rmdir(void) {
  char *fd = (char *)YAP_AtomName(YAP_AtomOfTerm(YAP_ARG1));
#if defined(__MINGW32__) || _MSC_VER
  if (_rmdir(fd) == -1) {
#else
  if (rmdir(fd) == -1) {
#endif
    /* return an error number */
    return (YAP_Unify(YAP_ARG2, YAP_MkIntTerm(errno)));
  }
  return (TRUE);
}

static YAP_Bool rename_file(void) {
  char *s1 = (char *)YAP_AtomName(YAP_AtomOfTerm(YAP_ARG1));
  char *s2 = (char *)YAP_AtomName(YAP_AtomOfTerm(YAP_ARG2));
#if HAVE_RENAME
  if (rename(s1, s2) == -1) {
    /* return an error number */
    return (YAP_Unify(YAP_ARG3, YAP_MkIntTerm(errno)));
  }
#endif
  return (TRUE);
}

static YAP_Bool read_link(void) {
#if HAVE_READLINK
  char *s1 = (char *)YAP_AtomName(YAP_AtomOfTerm(YAP_ARG1));
  char buf[MAXPATHLEN + 1];

  if (readlink(s1, buf, MAXPATHLEN) < 0)
    return false;

  /* return an error number */
  if (!YAP_Unify(YAP_ARG2, YAP_MkAtomTerm(YAP_LookupAtom(buf)))) {
    return false;
  }
#endif
#if _WIN32
  return false;
#endif
  return true;
}

static YAP_Bool dir_separator(void) {
  return (YAP_Unify(YAP_ARG1, YAP_MkAtomTerm(YAP_LookupAtom("/"))));
}

static YAP_Bool file_property(void) {
  const char *fd;
#if HAVE_LSTAT
  struct stat buf;

  fd = (char *)YAP_AtomName(YAP_AtomOfTerm(YAP_ARG1));
  if (lstat(fd, &buf) == -1) {
    /* return an error number */
    return (YAP_Unify(YAP_ARG7, YAP_MkIntTerm(errno)));
  }
  if (S_ISREG(buf.st_mode)) {
    if (!(YAP_Unify(YAP_ARG2, YAP_MkAtomTerm(YAP_LookupAtom("regular"))) &&
          YAP_Unify(YAP_ARG6, YAP_MkIntTerm(0))))
      return (FALSE);
  } else if (S_ISDIR(buf.st_mode)) {
    if (!(YAP_Unify(YAP_ARG2, YAP_MkAtomTerm(YAP_LookupAtom("directory"))) &&
          YAP_Unify(YAP_ARG6, YAP_MkIntTerm(0))))
      return (FALSE);
  } else if (S_ISFIFO(buf.st_mode)) {
    if (!(YAP_Unify(YAP_ARG2, YAP_MkAtomTerm(YAP_LookupAtom("fifo"))) &&
          YAP_Unify(YAP_ARG6, YAP_MkIntTerm(0))))
      return (FALSE);
  } else if (S_ISLNK(buf.st_mode)) {
    if (!YAP_Unify(YAP_ARG2, YAP_MkAtomTerm(YAP_LookupAtom("symlink"))))
      return (FALSE);
#if HAVE_READLINK
    {
      char tmp[256];
      int n;
      if ((n = readlink(fd, tmp, 256)) == -1) {
        return (YAP_Unify(YAP_ARG7, YAP_MkIntTerm(errno)));
      }
      tmp[n] = '\0';
      if (!YAP_Unify(YAP_ARG6, YAP_MkAtomTerm(YAP_LookupAtom(tmp)))) {
        return (FALSE);
      }
    }
#else
    if (!YAP_Unify(YAP_ARG6, YAP_MkIntTerm(0)))
      return (FALSE);
#endif
  } else if (S_ISSOCK(buf.st_mode)) {
    if (!(YAP_Unify(YAP_ARG2, YAP_MkAtomTerm(YAP_LookupAtom("socket"))) &&
          YAP_Unify(YAP_ARG6, YAP_MkIntTerm(0))))
      return (FALSE);
  } else {
    if (!(YAP_Unify(YAP_ARG2, YAP_MkAtomTerm(YAP_LookupAtom("unknown"))) &&
          YAP_Unify(YAP_ARG6, YAP_MkIntTerm(0))))
      return (FALSE);
  }
#elif defined(__MINGW32__) || _MSC_VER
  /* for some weird reason _stat did not work with mingw32 */
  struct _stat buf;

  fd = YAP_AtomName(YAP_AtomOfTerm(YAP_ARG1));
  if (_stat(fd, &buf) != 0) {
    /* return an error number */
    return (YAP_Unify(YAP_ARG7, YAP_MkIntTerm(errno)));
  }
  if (buf.st_mode & S_IFREG) {
    if (!YAP_Unify(YAP_ARG2, YAP_MkAtomTerm(YAP_LookupAtom("regular"))))
      return (FALSE);
  } else if (buf.st_mode & S_IFDIR) {
    if (!YAP_Unify(YAP_ARG2, YAP_MkAtomTerm(YAP_LookupAtom("directory"))))
      return (FALSE);
  } else {
    if (!YAP_Unify(YAP_ARG2, YAP_MkAtomTerm(YAP_LookupAtom("unknown"))))
      return (FALSE);
  }
#endif
  return (YAP_Unify(YAP_ARG3, YAP_MkIntTerm(buf.st_size)) &&
          YAP_Unify(YAP_ARG4, YAP_MkIntTerm(buf.st_mtime)) &&
          YAP_Unify(YAP_ARG5, YAP_MkIntTerm(buf.st_mode)));
}

/* temporary files */

static YAP_Bool p_mktemp(void) {
#if HAVE_MKSTEMP || HAVE_MKTEMP || defined(__MINGW32__) || _MSC_VER
  char *s, tmp[BUF_SIZE];
  s = (char *)YAP_AtomName(YAP_AtomOfTerm(YAP_ARG1));
#if HAVE_STRNCPY
  strncpy(tmp, s, BUF_SIZE);
#else
  strcpy(tmp, s);
#endif
#if defined(__MINGW32__) || _MSC_VER
  if ((s = _mktemp(tmp)) == NULL) {
    /* return an error number */
    return (YAP_Unify(YAP_ARG3, YAP_MkIntTerm(errno)));
  }
  return (YAP_Unify(YAP_ARG2, YAP_MkAtomTerm(YAP_LookupAtom(s))));
#elif HAVE_MKSTEMP
  strcpy(tmp, "/tmp/YAP_tmpXXXXXXXX");
  if (mkstemp(tmp) == -1) {
    /* return an error number */
    return (YAP_Unify(YAP_ARG3, YAP_MkIntTerm(errno)));
  }
  return YAP_Unify(YAP_ARG2, YAP_MkAtomTerm(YAP_LookupAtom(tmp)));
#else
  if ((s = mktemp(tmp)) == NULL) {
    /* return an error number */
    return (YAP_Unify(YAP_ARG3, YAP_MkIntTerm(errno)));
  }
  return YAP_Unify(YAP_ARG2, YAP_MkAtomTerm(YAP_LookupAtom(s)));
#endif
#else
  return FALSE;
#endif
  return (TRUE);
}

static YAP_Bool p_tmpnam(void) {
#if HAVE_MKSTEMP
  char s[21];
  strcpy(s, "/tmp/YAP_tmpXXXXXXXX");
  if (mkstemp(s) == -1)
    return FALSE;
  return YAP_Unify(YAP_ARG1, YAP_MkAtomTerm(YAP_LookupAtom(s)));
#elif HAVE_MKTEMP
  char *s;
  if (!(s = mktemp("/tmp/YAP_tmpXXXXXXXX")))
    return FALSE;
  return YAP_Unify(YAP_ARG1, YAP_MkAtomTerm(YAP_LookupAtom(s)));
#elif HAVE_TMPNAM
  char buf[L_tmpnam], *s;
  if (!(s = tmpnam(buf)))
    return FALSE;
  return YAP_Unify(YAP_ARG1, YAP_MkAtomTerm(YAP_LookupAtom(s)));
#else
  return FALSE;
#endif
}

static YAP_Bool p_tmpdir(void) {
#if defined(__MINGW32__) || _MSC_VER
  char buf[512];
  DWORD out = GetTempPath(512, buf);
  if (!out) {
    return (YAP_Unify(YAP_ARG2, WinError()));
  }
  if (out > 511) {
    char *nbuf = malloc(out + 1);
    if (!nbuf)
      return YAP_Unify(YAP_ARG2,
                       YAP_MkAtomTerm(YAP_LookupAtom("no malloc memory")));
    out = GetTempPath(512, nbuf);
    if (!out) {
      return YAP_Unify(YAP_ARG2, WinError());
    }
    return YAP_Unify(YAP_ARG1, YAP_MkAtomTerm(YAP_LookupAtom(nbuf)));
  }
  return YAP_Unify(YAP_ARG1, YAP_MkAtomTerm(YAP_LookupAtom(buf)));
#else
  char *s;
  if ((s = getenv("TMPDIR")))
    return YAP_Unify(YAP_ARG1, YAP_MkAtomTerm(YAP_LookupAtom(s)));
#ifdef P_tmpdir
  return YAP_Unify(YAP_ARG1, YAP_MkAtomTerm(YAP_LookupAtom(P_tmpdir)));
#endif
  return YAP_Unify(YAP_ARG1, YAP_MkAtomTerm(YAP_LookupAtom("/tmp")));
#endif
}

/* return YAP's environment */
static YAP_Bool p_environ(void) {
#if HAVE_ENVIRON && 0
#if HAVE__NSGETENVIRON
  char **ptr = _NSGetEnviron();
#elif defined(__MINGW32__) || _MSC_VER
  extern char **_environ;
  char **ptr = _environ;
#else
  extern char **environ;
  char **ptr = environ;
#endif
  YAP_Term t1 = YAP_ARG1;
  long int i;

  i = YAP_IntOfTerm(t1);
  if (ptr[i] == NULL)
    return (FALSE);
  else {
    YAP_Term t = YAP_BufferToString(ptr[i]);
    return (YAP_Unify(t, YAP_ARG2));
  }
#else
  YAP_Error(0, 0L, "environ not available in this configuration");
  return (FALSE);
#endif
}

#if defined(__MINGW32__) || _MSC_VER
static HANDLE get_handle(YAP_Term ti, DWORD fd) {
  if (YAP_IsAtomTerm(ti)) {
    HANDLE out;
    SECURITY_ATTRIBUTES satt;

    satt.nLength = sizeof(satt);
    satt.lpSecurityDescriptor = NULL;
    satt.bInheritHandle = TRUE;
    out = CreateFile("NUL", GENERIC_READ | GENERIC_WRITE,
                     FILE_SHARE_READ | FILE_SHARE_WRITE, &satt, OPEN_EXISTING,
                     0, NULL);
    return (out);
  } else {
    if (YAP_IsIntTerm(ti)) {
      return (GetStdHandle(fd));
    } else
      return ((HANDLE)_get_osfhandle(YAP_StreamToFileNo(ti)));
  }
}

static void close_handle(YAP_Term ti, HANDLE h) {
  if (YAP_IsAtomTerm(ti)) {
    CloseHandle(h);
  }
}

#endif

/* execute a command as a detached process */
static YAP_Bool execute_command(void) {
  YAP_Term ti = YAP_ARG2, to = YAP_ARG3, te = YAP_ARG4;
  int res;

#if defined(__MINGW32__) || _MSC_VER
  HANDLE inpf, outf, errf;
  DWORD CreationFlags = 0;
  STARTUPINFO StartupInfo;
  PROCESS_INFORMATION ProcessInformation;
  inpf = get_handle(ti, STD_INPUT_HANDLE);
  if (inpf == INVALID_HANDLE_VALUE) {
    return (YAP_Unify(YAP_ARG6, WinError()));
  }
  outf = get_handle(to, STD_OUTPUT_HANDLE);
  if (outf == INVALID_HANDLE_VALUE) {
    close_handle(ti, inpf);
    return (YAP_Unify(YAP_ARG6, WinError()));
  }
  errf = get_handle(te, STD_OUTPUT_HANDLE);
  if (errf == INVALID_HANDLE_VALUE) {
    close_handle(ti, inpf);
    close_handle(to, outf);
    return (YAP_Unify(YAP_ARG6, WinError()));
  }
  if (!YAP_IsIntTerm(ti) && !YAP_IsIntTerm(to) && !YAP_IsIntTerm(te)) {
    /* we do not keep a current stream */
    CreationFlags = DETACHED_PROCESS;
  }
  StartupInfo.cb = sizeof(STARTUPINFO);
  StartupInfo.lpReserved = NULL;
  StartupInfo.lpDesktop = NULL; /* inherit */
  StartupInfo.lpTitle = NULL;   /* we do not create a new console window */
  StartupInfo.dwFlags = STARTF_USESTDHANDLES;
  StartupInfo.cbReserved2 = 0;
  StartupInfo.lpReserved2 = NULL;
  StartupInfo.hStdInput = inpf;
  StartupInfo.hStdOutput = outf;
  StartupInfo.hStdError = errf;
  /* got stdin, stdout and error as I like it */
  if (CreateProcess(NULL, (char *)YAP_AtomName(YAP_AtomOfTerm(YAP_ARG1)), NULL,
                    NULL, TRUE, CreationFlags, NULL, NULL, &StartupInfo,
                    &ProcessInformation) == FALSE) {
    close_handle(ti, inpf);
    close_handle(to, outf);
    close_handle(te, errf);
    return (YAP_Unify(YAP_ARG6, WinError()));
  }
  close_handle(ti, inpf);
  close_handle(to, outf);
  close_handle(te, errf);
  res = ProcessInformation.dwProcessId;
  return (YAP_Unify(YAP_ARG5, YAP_MkIntTerm(res)));
#else  /* UNIX CODE */
  YAP_Term AtomNull = YAP_MkAtomTerm(YAP_LookupAtom("null"));
  int inpf, outf, errf;
  /* process input first */
  if (ti == AtomNull) {
    inpf = open("/dev/null", O_RDONLY);
  } else {
    int sd;
    if (YAP_IsIntTerm(ti))
      sd = YAP_IntOfTerm(ti);
    else
      sd = YAP_StreamToFileNo(ti);
    if (sd == 0)
      inpf = 0;
    else
      inpf = dup(sd);
  }
  if (inpf < 0) {
    /* return an error number */
    return (YAP_Unify(YAP_ARG6, YAP_MkIntTerm(errno)));
  }
  /* then output stream */
  if (to == AtomNull) {
    outf = open("/dev/zero", O_WRONLY);
  } else {
    int sd;
    if (YAP_IsIntTerm(to))
      sd = YAP_IntOfTerm(to);
    else
      sd = YAP_StreamToFileNo(to);
    if (sd == 1)
      outf = 1;
    else
      outf = dup(sd);
  }
  if (outf < 0) {
    /* return an error number */
    if (inpf != 0)
      close(inpf);
    return (YAP_Unify(YAP_ARG6, YAP_MkIntTerm(errno)));
  }
  /* then error stream */
  if (te == AtomNull) {
    errf = open("/dev/zero", O_WRONLY);
  } else {
    int sd;
    if (YAP_IsIntTerm(te))
      sd = YAP_IntOfTerm(te);
    else
      sd = YAP_StreamToFileNo(te);
    if (sd == 2)
      errf = 2;
    else
      errf = dup(sd);
  }
  if (errf < 0) {
    /* return an error number */
    if (inpf != 0)
      close(inpf);
    if (outf != 1)
      close(outf);
    return (YAP_Unify(YAP_ARG6, YAP_MkIntTerm(errno)));
  }
  YAP_FlushAllStreams();
  /* we are now ready to fork */
  if ((res = fork()) < 0) {
    /* close streams we don't need */
    if (inpf != 0)
      close(inpf);
    if (outf != 1)
      close(outf);
    if (errf != 2)
      close(errf);
    /* return an error number */
    return (YAP_Unify(YAP_ARG6, YAP_MkIntTerm(errno)));
  } else if (res == 0) {
    char *argv[4];

    /* child */
    /* close current streams, but not std streams */
    YAP_CloseAllOpenStreams();
    if (inpf != 0) {
      close(0);
      if (dup(inpf) != 0)
        exit(1);
      close(inpf);
    }
    if (outf != 1) {
      close(1);
      if (dup(outf) != 1)
        exit(1);
      close(outf);
    }
    if (errf != 2) {
      close(2);
      if (dup(errf) != 2)
        exit(2);
      close(errf);
    }
    argv[0] = "sh";
    argv[1] = "-c";
    argv[2] = (char *)YAP_AtomName(YAP_AtomOfTerm(YAP_ARG1));
    argv[3] = NULL;
    execv("/bin/sh", argv);
    exit(127);
    /* we have the streams where we want them, just want to execute now */
  } else {
    if (inpf != 0)
      close(inpf);
    if (outf != 1)
      close(outf);
    if (errf != 2)
      close(errf);
    return (YAP_Unify(YAP_ARG5, YAP_MkIntTerm(res)));
  }
#endif /* UNIX code */
}

/** @pred  system(+ _S_)

Passes command  _S_ to the Bourne shell (on UNIX environments) or the
current command interpreter in WIN32 environments.

Note that it executes them command as a detached process. It requires
`system` to be implemented by the system library.

*/
static YAP_Bool do_system(void) {
  char *command = (char *)YAP_AtomName(YAP_AtomOfTerm(YAP_ARG1));
#if HAVE_SYSTEM
  int sys = system(command);
  if (sys < 0) {
    return YAP_Unify(YAP_ARG3, YAP_MkIntTerm(errno));
  }
  return YAP_Unify(YAP_ARG2, YAP_MkIntTerm(sys));
#else
  YAP_Error(0, 0L, "system not available in this configuration, trying %s",
            command);
  return FALSE;
#endif
}

/* execute a command as a detached process */
static YAP_Bool do_shell(void) {
#if defined(__MINGW32__) || _MSC_VER
  YAP_Error(0, 0L, "system not available in this configuration");
  return (FALSE);
#elif HAVE_SYSTEM
  char *buf = YAP_AllocSpaceFromYap(BUF_SIZE);
  int sys;

  if (buf == NULL) {
    YAP_Error(0, 0L, "No Temporary Space for Shell");
    return (FALSE);
  }
#if HAVE_STRNCPY
  strncpy(buf, YAP_AtomName(YAP_AtomOfTerm(YAP_ARG1)), BUF_SIZE);
  strncpy(buf, " ", BUF_SIZE);
  strncpy(buf, YAP_AtomName(YAP_AtomOfTerm(YAP_ARG2)), BUF_SIZE);
  strncpy(buf, " ", BUF_SIZE);
  strncpy(buf, YAP_AtomName(YAP_AtomOfTerm(YAP_ARG3)), BUF_SIZE);
#else
  strcpy(buf, YAP_AtomName(YAP_AtomOfTerm(YAP_ARG1)));
  strcpy(buf, YAP_AtomName(YAP_AtomOfTerm(YAP_ARG2)));
  strcpy(buf, " ");
  strcpy(buf, YAP_AtomName(YAP_AtomOfTerm(YAP_ARG3)));
#endif
  sys = system(buf);
  YAP_FreeSpaceFromYap(buf);
  if (sys < 0) {
    return YAP_Unify(YAP_ARG5, YAP_MkIntTerm(errno));
  }
  return YAP_Unify(YAP_ARG4, YAP_MkIntTerm(sys));
#else
  char *cptr[4];
  int t;
  int sys;

  cptr[0] = (char *)YAP_AtomName(YAP_AtomOfTerm(YAP_ARG1));
  cptr[1] = (char *)YAP_AtomName(YAP_AtomOfTerm(YAP_ARG2));
  cptr[2] = (char *)YAP_AtomName(YAP_AtomOfTerm(YAP_ARG3));
  cptr[3] = NULL;
  t = fork();
  if (t < 0) {
    return YAP_Unify(YAP_ARG5, YAP_MkIntTerm(errno));
  } else if (t == 0) {
    t = execvp(cptr[0], cptr);
    return t;
  } else {
    t = wait(&sys);
    if (t < 0) {
      return YAP_Unify(YAP_ARG5, YAP_MkIntTerm(errno));
    }
  }
  return YAP_Unify(YAP_ARG4, YAP_MkIntTerm(sys));
#endif
}

/* execute a command as a detached process */
static YAP_Bool plwait(void) {
  long int pid = YAP_IntOfTerm(YAP_ARG1);
#if defined(__MINGW32__) || _MSC_VER
  HANDLE proc = OpenProcess(STANDARD_RIGHTS_REQUIRED | SYNCHRONIZE, FALSE, pid);
  DWORD ExitCode;
  if (proc == NULL) {
    return (YAP_Unify(YAP_ARG3, WinError()));
  }
  if (WaitForSingleObject(proc, INFINITE) == WAIT_FAILED) {
    return (YAP_Unify(YAP_ARG3, WinError()));
  }
  if (GetExitCodeProcess(proc, &ExitCode) == 0) {
    return (YAP_Unify(YAP_ARG4, WinError()));
  }
  CloseHandle(proc);
  return (YAP_Unify(YAP_ARG2, YAP_MkIntTerm(ExitCode)));
#else
  do {
    int status;

    /* check for interruptions */
    if (waitpid(pid, &status, 0) == -1) {
      if (errno) {
        if (errno == EINTR) {
          continue;
        }
        return YAP_Unify(YAP_ARG3, YAP_MkIntTerm(errno));
      }
    }
    if (WIFEXITED(status)) {
      return YAP_Unify(YAP_ARG2, YAP_MkIntTerm(WEXITSTATUS(status)));
    } else if (WIFSIGNALED(status)) {
      return YAP_Unify(YAP_ARG3, YAP_MkAtomTerm(YAP_LookupAtom("signalled"))) &&
             YAP_Unify(YAP_ARG4, YAP_MkIntTerm(WTERMSIG(status)));
    } else /* WIFSTOPPED(status) */ {
      return YAP_Unify(YAP_ARG3, YAP_MkAtomTerm(YAP_LookupAtom("stopped"))) &&
             YAP_Unify(YAP_ARG4, YAP_MkIntTerm(WSTOPSIG(status)));
    }
  } while (TRUE);
#endif
}

/* host info */

static YAP_Bool host_name(void) {
#if defined(__MINGW32__) || _MSC_VER
  char name[MAX_COMPUTERNAME_LENGTH + 1];
  DWORD nSize = MAX_COMPUTERNAME_LENGTH + 1;
  if (GetComputerName(name, &nSize) == 0) {
    return (YAP_Unify(YAP_ARG2, WinError()));
  }
#else
#if HAVE_GETHOSTNAME
  char name[256];
  if (gethostname(name, 256) == -1) {
    /* return an error number */
    return (YAP_Unify(YAP_ARG2, YAP_MkIntTerm(errno)));
  }
#endif
#endif /* defined(__MINGW32__) || _MSC_VER */
  return (YAP_Unify(YAP_ARG1, YAP_MkAtomTerm(YAP_LookupAtom(name))));
}

static YAP_Bool host_id(void) {
#if HAVE_GETHOSTID
  return (YAP_Unify(YAP_ARG1, YAP_MkIntTerm(gethostid())));
#else
  return (YAP_Unify(YAP_ARG1, YAP_MkIntTerm(0)));
#endif
}

static YAP_Bool pid(void) {
#if defined(__MINGW32__) || _MSC_VER
  return (YAP_Unify(YAP_ARG1, YAP_MkIntTerm(_getpid())));
#else
  return (YAP_Unify(YAP_ARG1, YAP_MkIntTerm(getpid())));
#endif
}

static YAP_Bool win(void) {
#if defined(__MINGW32__) || _MSC_VER
  return (TRUE);
#else
  return (FALSE);
#endif
}

static YAP_Bool p_kill(void) {
#if defined(__MINGW32__) || _MSC_VER
  /* Windows does not support cross-process signals, so we shall do the
     SICStus thing and assume that a signal to a process will
     always kill it */
  HANDLE proc = OpenProcess(STANDARD_RIGHTS_REQUIRED | PROCESS_TERMINATE, FALSE,
                            YAP_IntOfTerm(YAP_ARG1));
  if (proc == NULL) {
    return (YAP_Unify(YAP_ARG3, WinError()));
  }
  if (TerminateProcess(proc, -1) == 0) {
    return (YAP_Unify(YAP_ARG3, WinError()));
  }
  CloseHandle(proc);
#else
  if (kill(YAP_IntOfTerm(YAP_ARG1), YAP_IntOfTerm(YAP_ARG2)) < 0) {
    /* return an error number */
    return (YAP_Unify(YAP_ARG3, YAP_MkIntTerm(errno)));
  }
#endif /* defined(__MINGW32__) || _MSC_VER */
  return (TRUE);
}

#if HAVE_OPENSSL_RIPEMD_H
#include <openssl/ripemd.h>
#endif

/** md5( +Text, -Key, -Remaining keyq
 * encode text using OpenSSL
 *
 * arg Text is a List of ASCII codes
 * arg2 and 3: difference list with the character codes for the
 * digest.
 *
 * @return whether ARG1's md5 unifies with the difference liat.
 */
static YAP_Bool md5(void) {
  unsigned char buf[64];
  md5_state_t pms;
  const char *s;
  size_t len = -1;

  if (!(s = YAP_StringToBuffer(YAP_ARG1, NULL, len)) || s[0] == 0)
    return false;

  md5_init(&pms);
  md5_append(&pms, (const unsigned char *)s, strlen(s));
  md5_finish(&pms, buf);
  // free((void *)s);
  YAP_Term t = YAP_ARG3;
  int i = 16;
  while (i > 0) {
    int top, bop;
    i--;
    top = buf[i] >> 4;
    if (top > 9)
      top = (top - 10) + 'a';
    else
      top = top + '0';
    bop = buf[i] & 15;
    if (bop > 9)
      bop = (bop - 10) + 'a';
    else
      bop = bop + '0';
    t = YAP_MkPairTerm(YAP_MkIntTerm(top),
                       YAP_MkPairTerm(YAP_MkIntTerm(bop), t));
  }
  return YAP_Unify(YAP_ARG2, t);
}

static YAP_Bool error_message(void) {
#if HAVE_STRERROR
  return YAP_Unify(YAP_ARG2, YAP_MkAtomTerm(YAP_LookupAtom(
                                 strerror(YAP_IntOfTerm(YAP_ARG1)))));
#else
  return YAP_Unify(YAP_ARG2, YAP_ARG1);
#endif
}
X_API void init_sys(void) {
#if HAVE_MKTIME
  tzset();
#endif
  YAP_UserCPredicate("datime", datime, 2);
  YAP_UserCPredicate("mktime", sysmktime, 8);
  YAP_UserCPredicate("file_property", file_property, 7);
  YAP_UserCPredicate("unlink", p_unlink, 2);
  YAP_UserCPredicate("rmdir", p_rmdir, 2);
  YAP_UserCPredicate("dir_separator", dir_separator, 1);
  YAP_UserCPredicate("p_environ", p_environ, 2);
  YAP_UserCPredicate("exec_command", execute_command, 6);
  YAP_UserCPredicate("do_shell", do_shell, 5);
  YAP_UserCPredicate("do_system", do_system, 3);
  YAP_UserCPredicate("plwait", plwait, 4);
  YAP_UserCPredicate("host_name", host_name, 2);
  YAP_UserCPredicate("host_id", host_id, 2);
  YAP_UserCPredicate("pid", pid, 2);
  YAP_UserCPredicate("kill", p_kill, 3);
  YAP_UserCPredicate("mktemp", p_mktemp, 3);
  YAP_UserCPredicate("tmpnam", p_tmpnam, 2);
  YAP_UserCPredicate("tmpdir", p_tmpdir, 2);
  YAP_UserCPredicate("rename_file", rename_file, 3);
  YAP_UserCPredicate("read_link", read_link, 2);
  YAP_UserCPredicate("error_message", error_message, 2);
  YAP_UserCPredicate("win", win, 0);
  YAP_UserCPredicate("md5", md5, 3);
}

#ifdef _WIN32

#include <windows.h>

int WINAPI win_sys(HANDLE, DWORD, LPVOID);

int WINAPI win_sys(HANDLE hinst, DWORD reason, LPVOID reserved) {
  switch (reason) {
  case DLL_PROCESS_ATTACH:
    break;
  case DLL_PROCESS_DETACH:
    break;
  case DLL_THREAD_ATTACH:
    break;
  case DLL_THREAD_DETACH:
    break;
  }
  return 1;
}
#endif
