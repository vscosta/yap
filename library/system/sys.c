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
* File:		regexp.c						 *
* Last rev:								 *
* mods:									 *
* comments:	regular expression interpreter                           *
*									 *
*************************************************************************/

#include "config.h"
#include "c_interface.h"
#if STDC_HEADERS
#include <stdlib.h>
#endif
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
#include <windows.h>
#include <process.h>
#endif
#ifdef __MINGW32__
#ifdef HAVE_ENVIRON
#undef HAVE_ENVIRON
#endif
#endif

void PROTO(init_sys, (void));

#if defined(__MINGW32__) || _MSC_VER
static Term
WinError(void)
{
  char msg[256];
  /* Error, we could not read time */
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
		  NULL, GetLastError(), 
		  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), msg, 256,
		  NULL);
    return(MkAtomTerm(LookupAtom(msg)));
}
#endif

/* Return time in a structure */
static int
datime(void)
{
  Term tf, out[6];
#if defined(__MINGW32__) || _MSC_VER
  SYSTEMTIME stime;
  GetLocalTime(&stime);
  out[0] = MkIntTerm(stime.wYear);
  out[1] = MkIntTerm(stime.wMonth);
  out[2] = MkIntTerm(stime.wDay);
  out[3] = MkIntTerm(stime.wHour);
  out[4] = MkIntTerm(stime.wMinute);
  out[5] = MkIntTerm(stime.wSecond);
#elif HAVE_TIME
  time_t  tp;

  if ((tp = time(NULL)) == -1) {
     return(unify(ARG2, MkIntTerm(errno)));    
  }
#ifdef HAVE_LOCALTIME
 {
   struct tm *loc = localtime(&tp);
   if (loc == NULL) {
     return(unify(ARG2, MkIntTerm(errno)));    
   }
   out[0] = MkIntTerm(1900+loc->tm_year);
   out[1] = MkIntTerm(1+loc->tm_mon);
   out[2] = MkIntTerm(loc->tm_mday);
   out[3] = MkIntTerm(loc->tm_hour);
   out[4] = MkIntTerm(loc->tm_min);
   out[5] = MkIntTerm(loc->tm_sec);
 }
#else
  oops
#endif /* HAVE_LOCALTIME */
#else
  oops
#endif /* HAVE_TIME */
  tf = MkApplTerm(MkFunctor(LookupAtom("datime"),6), 6, out);
  return(unify(ARG1, tf));
}

#define BUF_SIZE 1024

/* Return a list of files for a directory */
static int
list_directory(void)
{
  Term tf = MkAtomTerm(LookupAtom("[]"));

  char *buf = AtomName(AtomOfTerm(ARG1));
#if defined(__MINGW32__) || _MSC_VER
  struct _finddata_t c_file;
  char bs[BUF_SIZE];
  long hFile;

  bs[0] = '\0';
#if HAVE_STRNCPY
  strncpy(bs, buf, BUF_SIZE);
#else
  strcpy(bs, buf);
#endif
#if HAVE_STRNCAT
  strncat(bs, "/*", BUF_SIZE);
#else
  strncat(bs, "/*");
#endif
  if ((hFile = _findfirst(bs, &c_file)) == -1L) {
    return(unify(ARG2,tf));
  }
  tf = MkPairTerm(MkAtomTerm(LookupAtom(c_file.name)), tf);
  while (_findnext( hFile, &c_file) == 0) {
    Term ti = MkAtomTerm(LookupAtom(c_file.name));
    tf = MkPairTerm(ti, tf);
  }
  _findclose( hFile );
#else
#if HAVE_OPENDIR
 {
   DIR *de;
   struct dirent *dp;

   if ((de = opendir(buf)) == NULL) {
     return(unify(ARG3, MkIntTerm(errno)));
   }
   while ((dp = readdir(de))) {
     Term ti = MkAtomTerm(LookupAtom(dp->d_name));
     tf = MkPairTerm(ti, tf);
   }
   closedir(de);
 }
#endif /* HAVE_OPENDIR */
#endif
  return(unify(ARG2, tf));
}

static int
p_unlink(void)
{
  char *fd = AtomName(AtomOfTerm(ARG1));
#if defined(__MINGW32__) || _MSC_VER
  if (_unlink(fd) == -1)
#else
  if (unlink(fd) == -1)
#endif
    {
      /* return an error number */
      return(unify(ARG2, MkIntTerm(errno)));
    }
  return(TRUE);
}

static int
p_mkdir(void)
{
  char *fd = AtomName(AtomOfTerm(ARG1));
#if defined(__MINGW32__) || _MSC_VER
  if (_mkdir(fd) == -1) {
#else
  if (mkdir(fd, 0777) == -1) {
#endif
    /* return an error number */
    return(unify(ARG2, MkIntTerm(errno)));
  }
  return(TRUE);
}

static int
p_rmdir(void)
{
  char *fd = AtomName(AtomOfTerm(ARG1));
#if defined(__MINGW32__) || _MSC_VER
  if (_rmdir(fd) == -1) {
#else
  if (rmdir(fd) == -1) {
#endif
    /* return an error number */
    return(unify(ARG2, MkIntTerm(errno)));
  }
  return(TRUE);
}

static int
rename_file(void)
{
  char *s1 = AtomName(AtomOfTerm(ARG1));
  char *s2 = AtomName(AtomOfTerm(ARG2));
#if HAVE_RENAME
  if (rename(s1, s2) == -1) {
    /* return an error number */
    return(unify(ARG3, MkIntTerm(errno)));
  }
#endif
  return(TRUE);
}

static int
dir_separator(void)
{
  return(unify(ARG1,MkAtomTerm(LookupAtom("/"))));
}

static int
file_property(void)
{
  char *fd;
#if HAVE_LSTAT 
  struct stat buf;

  fd = AtomName(AtomOfTerm(ARG1));
  if (lstat(fd, &buf) == -1) {
    /* return an error number */
    return(unify(ARG7, MkIntTerm(errno)));
  }
  if (S_ISREG(buf.st_mode)) {
    if (!(unify(ARG2, MkAtomTerm(LookupAtom("regular"))) &&
	  unify(ARG6, YapMkIntTerm(0))))
      return(FALSE);
  } else if (S_ISDIR(buf.st_mode)) {
    if (!(unify(ARG2, MkAtomTerm(LookupAtom("directory"))) &&
	  unify(ARG6, YapMkIntTerm(0))))
      return(FALSE);
  } else if (S_ISFIFO(buf.st_mode)) {
    if (!(unify(ARG2, MkAtomTerm(LookupAtom("fifo"))) &&
	  unify(ARG6, YapMkIntTerm(0))))
      return(FALSE);
  } else if (S_ISLNK(buf.st_mode)) {
    if (!unify(ARG2, MkAtomTerm(LookupAtom("symlink"))))
      return(FALSE);
#if HAVE_READLINK
    {
      char tmp[256];
      if (readlink(fd,tmp,256) == -1) {
	return(unify(ARG7, MkIntTerm(errno)));
      }
      if(!unify(ARG6,MkAtomTerm(LookupAtom(tmp)))) {
	return(FALSE);
      }
    }
#else
    if (!unify(ARG6, YapMkIntTerm(0)))
      return(FALSE);
#endif    
  } else if (S_ISSOCK(buf.st_mode)) {
    if (!(unify(ARG2, MkAtomTerm(LookupAtom("socket"))) &&
	  unify(ARG6, YapMkIntTerm(0))))
      return(FALSE);
  } else {
    if (!(unify(ARG2, MkAtomTerm(LookupAtom("unknown"))) &&
	  unify(ARG6, YapMkIntTerm(0))))
      return(FALSE);
  }
#elif defined(__MINGW32__) || _MSC_VER
  /* for some weird reason _stat did not work with mingw32 */
  struct stat buf;

  fd = AtomName(AtomOfTerm(ARG1));
  if (stat(fd, &buf) != 0) {
    /* return an error number */
    return(unify(ARG7, MkIntTerm(errno)));
  }
  if (buf.st_mode & S_IFREG) {
    if (!unify(ARG2, MkAtomTerm(LookupAtom("regular"))))
      return(FALSE);
  } else if (buf.st_mode & S_IFDIR) {
    if (!unify(ARG2, MkAtomTerm(LookupAtom("directory"))))
      return(FALSE);
  } else {
    if (!unify(ARG2, MkAtomTerm(LookupAtom("unknown"))))
      return(FALSE);
  }
#endif
  return (
	  unify(ARG3, MkIntTerm(buf.st_size)) &&
	  unify(ARG4, MkIntTerm(buf.st_mtime)) &&
	  unify(ARG5, MkIntTerm(buf.st_mode))
	  );
}

/* temporary files */

static int
p_mktemp(void)
{
#if HAVE_MKTEMP
  char *s, tmp[1024];
  s = AtomName(AtomOfTerm(ARG1));
#if HAVE_STRNCPY
  strncpy(tmp, s, 1024);
#else
  strcpy(tmp, s);
#endif
#if defined(__MINGW32__) || _MSC_VER
  if ((s = _mktemp(tmp)) == NULL) {
#else
  if ((s = mktemp(tmp)) == NULL) {
#endif
    /* return an error number */
    return(unify(ARG3, MkIntTerm(errno)));
  }
  return(unify(ARG2,MkAtomTerm(LookupAtom(s))));
#else
 oops
#endif
  return(TRUE); 
}

static int
p_tpmnam(void)
{
#if HAVE_TMPNAM
  return(unify(ARG1,MkAtomTerm(LookupAtom(tmpnam(NULL)))));
#else
oops
#endif
}

/* return YAP's environment */
static int
p_environ(void)
{
#if HAVE_ENVIRON
#if defined(__MINGW32__) || _MSC_VER
  extern char **_environ;
#else
  extern char **environ;
#endif
  Term t1 = ARG1;
  Int i;

  i = IntOfTerm(t1);
#if defined(__MINGW32__) || _MSC_VER
  if (_environ[i] == NULL)
#else
  if (environ[i] == NULL)
#endif
    return(FALSE);
  else {
    Term t = BufferToString(environ[i]);
    return(unify(t, ARG2));
  }
#else
  YapError("environ not available in this configuration");
  return(FALSE);
#endif
}

#if defined(__MINGW32__) || _MSC_VER
static HANDLE
get_handle(Term ti, DWORD fd)
{
  if (IsAtomTerm(ti)) {
    HANDLE out;
    SECURITY_ATTRIBUTES satt;
    
    satt.nLength = sizeof(satt);
    satt.lpSecurityDescriptor = NULL;
    satt.bInheritHandle = TRUE;
    out = CreateFile("NUL",
			    GENERIC_READ|GENERIC_WRITE,
			    FILE_SHARE_READ|FILE_SHARE_WRITE,
			    &satt,
			    OPEN_EXISTING,
			    0,
			    NULL);
    return(out);
  } else {
    if (IsIntTerm(ti)) {
      return(GetStdHandle(fd));
    } else
      return((HANDLE)YapStreamToFileNo(ti));
  }
}

static void
close_handle(Term ti, HANDLE h)
{
  if (IsAtomTerm(ti)) {
    CloseHandle(h);
  }
}

#endif

/* execute a command as a detached process */
static int
execute_command(void)
{
  Term ti = ARG2, to = ARG3, te = ARG4;
  int res;
#if defined(__MINGW32__) || _MSC_VER
  HANDLE inpf, outf, errf;
  DWORD CreationFlags = 0;
  STARTUPINFO StartupInfo;
  PROCESS_INFORMATION ProcessInformation;
  inpf = get_handle(ti, STD_INPUT_HANDLE);
  if (inpf == INVALID_HANDLE_VALUE) {
    return(unify(ARG6, WinError()));
  }
  outf = get_handle(to, STD_OUTPUT_HANDLE);
  if (outf == INVALID_HANDLE_VALUE) {
    close_handle(ti, inpf);
    return(unify(ARG6, WinError()));
  }
  errf = get_handle(te, STD_OUTPUT_HANDLE);
  if (errf == INVALID_HANDLE_VALUE) {
    close_handle(ti, inpf);
    close_handle(to, outf);
    return(unify(ARG6, WinError()));
  }
  if (!IsIntTerm(ti) && !IsIntTerm(to) && !IsIntTerm(te)) {
    /* we do not keep a current stream */
    CreationFlags = DETACHED_PROCESS;
  }
  StartupInfo.cb = sizeof(STARTUPINFO);
  StartupInfo.lpReserved = NULL;
  StartupInfo.lpDesktop = NULL; /* inherit */
  StartupInfo.lpTitle = NULL; /* we do not create a new console window */
  StartupInfo.dwFlags = STARTF_USESTDHANDLES;
  StartupInfo.cbReserved2 = 0;
  StartupInfo.lpReserved2 = NULL;
  StartupInfo.hStdInput = inpf;
  StartupInfo.hStdOutput = outf;
  StartupInfo.hStdError = errf;
  /* got stdin, stdout and error as I like it */
  if (CreateProcess(NULL,
		    AtomName(AtomOfTerm(ARG1)),
		    NULL,
		    NULL,
		    TRUE,
		    CreationFlags,
		    NULL,
		    NULL,
		    &StartupInfo,
		    &ProcessInformation) == FALSE) {
    close_handle(ti, inpf);
    close_handle(to, outf);
    close_handle(te, errf);
    return(unify(ARG6, WinError()));
  }
  close_handle(ti, inpf);
  close_handle(to, outf);
  close_handle(te, errf);
  res = ProcessInformation.dwProcessId;
  return(unify(ARG5,MkIntTerm(res)));  
#else /* UNIX CODE */
  int inpf, outf, errf;
  /* process input first */
  if (IsAtomTerm(ti)) {
    inpf = open("/dev/null", O_RDONLY);
  } else {
    int sd;
    if (IsIntTerm(ti))
      sd = 0;
    else
      sd = YapStreamToFileNo(ti);
    inpf = dup(sd);
  }
  if (inpf < 0) {
    /* return an error number */
    return(unify(ARG6, MkIntTerm(errno)));
  }
  /* then output stream */
  if (IsAtomTerm(to)) {
    outf = open("/dev/zero", O_WRONLY);
  } else {
    int sd;
    if (IsIntTerm(to))
      sd = 1;
    else
      sd = YapStreamToFileNo(to);
    outf = dup(sd);
  }
  if (outf < 0) {
    /* return an error number */
    close(inpf);
    return(unify(ARG6, MkIntTerm(errno)));
  }
  /* then error stream */
  if (IsAtomTerm(te)) {
    errf = open("/dev/zero", O_WRONLY);
  } else {
    int sd;
    if (IsIntTerm(te))
      sd = 2;
    else
      sd = YapStreamToFileNo(te);
    errf = dup(sd);
  }
  if (errf < 0) {
    /* return an error number */
    close(inpf);
    close(outf);
    return(unify(ARG6, MkIntTerm(errno)));
  }
  /* we are now ready to fork */
  if ((res = fork()) < 0) {
    /* close streams we don't need */
    close(inpf);
    close(outf);
    close(errf);
    /* return an error number */
    return(unify(ARG6, MkIntTerm(errno)));
  } else if (res == 0) {
    char *argv[4];

    /* child */
    /* close current streams, but not std streams */
    YapCloseAllOpenStreams();
    close(0);
    dup(inpf);
    close(inpf);
    close(1);
    dup(outf);
    close(outf);
    close(2);
    dup(errf);
    close(errf);
    argv[0] = "sh";
    argv[1] = "-c";
    argv[2] = AtomName(AtomOfTerm(ARG1));
    argv[3] = NULL;
    execv("/bin/sh", argv);
    exit(127);
    /* we have the streams where we want them, just want to execute now */
  } else {
    close(inpf);
    close(outf);
    close(errf);
    return(unify(ARG5,MkIntTerm(res)));
  }
#endif /* UNIX code */
}

/* execute a command as a detached process */
static int
do_system(void)
{
  char *command = AtomName(AtomOfTerm(ARG1));
  int sys = system(command);
#if HAVE_SYSTEM
  if (sys < 0) {
    return(unify(ARG3,MkIntTerm(errno)));
  }
  return(unify(ARG2, MkIntTerm(sys)));
#endif
}

/* execute a command as a detached process */
static int
p_wait(void)
{
  Int pid = IntOfTerm(ARG1);
#if defined(__MINGW32__) || _MSC_VER
  HANDLE proc = OpenProcess(STANDARD_RIGHTS_REQUIRED|SYNCHRONIZE, FALSE, pid);
  DWORD ExitCode;
  if (proc == NULL) {
    return(unify(ARG3, WinError()));
  }
  if (WaitForSingleObject(proc, INFINITE) == WAIT_FAILED) {
    return(unify(ARG3, WinError()));
  }
  if (GetExitCodeProcess(proc, &ExitCode) == 0) {
    return(unify(ARG3, WinError()));
  }
  CloseHandle(proc);
  return(unify(ARG2, MkIntTerm(ExitCode)));
#else
  do {
    int status;

    /* check for interruptions */
    if (waitpid(pid, &status, 0) == -1) {
      if (errno != EINTR)
	return -1;
      return(unify(ARG3, MkIntTerm(errno)));
    } else {
      return(unify(ARG2, MkIntTerm(status)));
    }
  } while(TRUE);
#endif
}

/* execute a command as a detached process */
static int
p_popen(void)
{
  char *command = AtomName(AtomOfTerm(ARG1));
  Int mode = IntOfTerm(ARG2);
  FILE *pfd;
  Term tsno;
  int flags;
  
#if HAVE_POPEN
#if defined(__MINGW32__) || _MSC_VER
  /* This will only work for console applications. FIX */
  if (mode == 0)
    pfd = _popen(command, "r");
  else
    pfd = _popen(command, "w");
#else
  if (mode == 0)
    pfd = popen(command, "r");
  else
    pfd = popen(command, "w");
#endif
  if (pfd == NULL) {
    return(unify(ARG4, MkIntTerm(errno)));    
  }
  if (mode == 0)
    flags = YAP_INPUT_STREAM | YAP_POPEN_STREAM;
  else
    flags = YAP_OUTPUT_STREAM | YAP_POPEN_STREAM;
  tsno = YapOpenStream((void *)pfd,
		       "pipe",
		       MkAtomTerm(LookupAtom("pipe")),
		       flags);
#endif
  return(unify(ARG3, tsno));
}

static int
p_sleep(void)
{
  Term ts = ARG1;
  Int secs = 0, usecs = 0, out;
  if (IsIntTerm(ts)) {
    secs = IntOfTerm(ts);
  }  else if (IsFloatTerm(ts)) {
    flt tfl = FloatOfTerm(ts);
    if (tfl > 1.0)
      secs = tfl;
    else
      usecs = tfl*1000;
  }
#if defined(__MINGW32__) || _MSC_VER
  if (secs) usecs = secs*1000;
  Sleep(usecs);
  out = 0;
#else
#if HAVE_USLEEP
  if (usecs > 0) {
    usleep(usecs);
    out = 0;
  } else
#endif
#if HAVE_SLEEP
    {
      out = sleep(secs);
    }
#endif
#endif /* defined(__MINGW32__) || _MSC_VER */
  return(unify(ARG2, MkIntTerm(out)));
}

/* host info */

static int
host_name(void)
{
#if defined(__MINGW32__) || _MSC_VER
  char name[MAX_COMPUTERNAME_LENGTH+1];
  DWORD nSize = MAX_COMPUTERNAME_LENGTH+1;
  if (GetComputerName(name, &nSize) == 0) {
    return(unify(ARG2, WinError()));
  }
#else
#if HAVE_GETHOSTNAME
  char name[256];
  if (gethostname(name, 256) == -1) {
    /* return an error number */
    return(unify(ARG2, MkIntTerm(errno)));
  }
#endif
#endif /* defined(__MINGW32__) || _MSC_VER */
  return(unify(ARG1, MkAtomTerm(LookupAtom(name))));
}

static int
host_id(void)
{
#if HAVE_GETHOSTID
  return(unify(ARG1, MkIntTerm(gethostid())));
#else
  return(unify(ARG1, MkIntTerm(0)));
#endif
}

static int
pid(void)
{
#if defined(__MINGW32__) || _MSC_VER
  return(unify(ARG1, MkIntTerm(_getpid())));
#else
  return(unify(ARG1, MkIntTerm(getpid())));
#endif
}

static int
win(void)
{
#if defined(__MINGW32__) || _MSC_VER
  return(TRUE);
#else
  return(FALSE);
#endif
}

static int
p_kill(void)
{
#if defined(__MINGW32__) || _MSC_VER
  /* Windows does not support cross-process signals, so we shall do the
     SICStus thing and assume that a signal to a process will
     always kill it */
  HANDLE proc = OpenProcess(STANDARD_RIGHTS_REQUIRED|PROCESS_TERMINATE, FALSE, IntOfTerm(ARG1));
  if (proc == NULL) {
    return(unify(ARG3, WinError()));
  }
  if (TerminateProcess(proc, -1) == 0) {
    return(unify(ARG3, WinError()));
  }
  CloseHandle(proc);
#else
  if (kill(IntOfTerm(ARG1), IntOfTerm(ARG2)) < 0) {
    /* return an error number */
    return(unify(ARG3, MkIntTerm(errno)));
  }
#endif /* defined(__MINGW32__) || _MSC_VER */
  return(TRUE); 
}

static int
error_message(void)
{
#if HAVE_STRERROR
  return(unify(ARG2,MkAtomTerm(LookupAtom(strerror(IntOfTerm(ARG1))))));
#else
#if HAVE_STRERROR
  return(unify(ARG2,ARG1));
#endif
#endif
}

void
init_sys(void)
{
  UserCPredicate("datime", datime, 2);
  UserCPredicate("list_directory", list_directory, 3);
  UserCPredicate("file_property", file_property, 7);
  UserCPredicate("unlink", p_unlink, 2);
  UserCPredicate("mkdir", p_mkdir, 2);
  UserCPredicate("rmdir", p_rmdir, 2);
  UserCPredicate("dir_separator", dir_separator, 1);
  UserCPredicate("p_environ", p_environ, 2);
  UserCPredicate("exec_command", execute_command, 6);
  UserCPredicate("do_system", do_system, 3);
  UserCPredicate("popen", p_popen, 4);
  UserCPredicate("wait", p_wait, 3);
  UserCPredicate("host_name", host_name, 2);
  UserCPredicate("host_id", host_id, 2);
  UserCPredicate("pid", pid, 2);
  UserCPredicate("kill", p_kill, 3);
  UserCPredicate("mktemp", p_mktemp, 3);
  UserCPredicate("tmpnam", p_tpmnam, 2);
  UserCPredicate("rename_file", rename_file, 3);
  UserCPredicate("sleep", p_sleep, 2);
  UserCPredicate("error_message", error_message, 2);
  UserCPredicate("win", win, 0);
}

#ifdef _WIN32

#include <windows.h>

int WINAPI PROTO(win_sys, (HANDLE, DWORD, LPVOID));

int WINAPI win_sys(HANDLE hinst, DWORD reason, LPVOID reserved)
{
  switch (reason) 
    {
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
