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

#ifdef __MINGW32__
#ifdef HAVE_ENVIRON
#undef HAVE_ENVIRON
#endif
#endif

void PROTO(init_sys, (void));

/* Return time in a structure */
static int
datime(void)
{
  Term tf, out[6];
#ifdef HAVE_TIME
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
   out[0] = MkIntTerm(loc->tm_year);
   out[1] = MkIntTerm(loc->tm_mon);
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
  return(unify(ARG2, tf));
}

static int
p_unlink(void)
{
  char *fd = AtomName(AtomOfTerm(ARG1));
  if (unlink(fd) == -1) {
    /* return an error number */
    return(unify(ARG2, MkIntTerm(errno)));
  }
  return(TRUE);
}

static int
p_mkdir(void)
{
  char *fd = AtomName(AtomOfTerm(ARG1));
  if (mkdir(fd, 0777) == -1) {
    /* return an error number */
    return(unify(ARG2, MkIntTerm(errno)));
  }
  return(TRUE);
}

static int
p_rmdir(void)
{
  char *fd = AtomName(AtomOfTerm(ARG1));
  if (rmdir(fd) == -1) {
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
    return(unify(ARG6, MkIntTerm(errno)));
  }
  if (S_ISREG(buf.st_mode)) 
    unify(ARG2, MkAtomTerm(LookupAtom("regular")));
  else if (S_ISDIR(buf.st_mode)) 
    unify(ARG2, MkAtomTerm(LookupAtom("directory")));
  else if (S_ISFIFO(buf.st_mode)) 
    unify(ARG2, MkAtomTerm(LookupAtom("fifo")));
  else if (S_ISLNK(buf.st_mode))
    unify(ARG2, MkAtomTerm(LookupAtom("symlink")));
  else if (S_ISSOCK(buf.st_mode))
    unify(ARG2, MkAtomTerm(LookupAtom("socket")));
  else 
    unify(ARG2, MkAtomTerm(LookupAtom("unknown")));
  unify(ARG3, MkIntTerm(buf.st_size));
  unify(ARG4, MkIntTerm(buf.st_mtime));
  unify(ARG5, MkIntTerm(buf.st_mode));
#endif
  return(TRUE);
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
  if ((s = mktemp(tmp)) == NULL) {
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
  extern char **environ;
  Term t1 = ARG1;
  Int i;

  i = IntOfTerm(t1);
  if (environ[i] == NULL)
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

/* execute a command as a detached process */
static int
execute_command(void)
{
  Term ti = ARG2, to = ARG3, te = ARG4;
  Term tzero = MkIntTerm(0);
  int res;
  int inpf, outf, errf;
  /* process input first */
  if (ti == tzero) {
    inpf = open("/dev/null", O_RDONLY);
  } else {
    int sd = YapStreamToFileNo(ti);
    inpf = dup(sd);
  }
  if (inpf < 0) {
    /* return an error number */
    return(unify(ARG6, MkIntTerm(errno)));
  }
  /* then output stream */
  if (to == tzero) {
    outf = open("/dev/zero", O_WRONLY);
  } else {
    int sd = YapStreamToFileNo(to);
    outf = dup(sd);
  }
  if (outf < 0) {
    /* return an error number */
    return(unify(ARG6, MkIntTerm(errno)));
  }
  /* then error stream */
  if (te == tzero) {
    errf = open("/dev/zero", O_WRONLY);
  } else {
    int sd = YapStreamToFileNo(te);
    errf = dup(sd);
  }
  if (errf < 0) {
    /* return an error number */
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
    close(1);
    dup(outf);
    close(2);
    dup(outf);
    close(inpf);
    close(outf);
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
}

/* execute a command as a detached process */
static int
shell(void)
{
  char *command = AtomName(AtomOfTerm(ARG1));
  int pid;
  /* we are now ready to fork */
  if ((pid = fork()) < 0) {
    /* return an error number */
    return(unify(ARG2, MkIntTerm(errno)));
  } else if (pid == 0) {
    char *argv[4];
    char *shell;

    /* child */
    /* close current streams, but not std streams */
    YapCloseAllOpenStreams();
#if HAVE_GETENV
    shell = getenv ("SHELL");
    if (shell == NULL)
      shell = "/bin/sh";
#endif
    argv[0] = shell;
    argv[1] = "-c";
    argv[2] = command;
    argv[3] = NULL;
    execv("/bin/sh", argv);
    exit(127);
    /* we have the streams where we want them, just want to execute now */
  } else {
    do {
      int status;

      /* check for interruptions */
      if (waitpid(pid, &status, 0) == -1) {
	if (errno != EINTR)
	  return -1;
	return(unify(ARG2, MkIntTerm(errno)));
      } else
	return(TRUE);
    } while(TRUE);
  }
}

/* execute a command as a detached process */
static int
p_system(void)
{
  char *command = AtomName(AtomOfTerm(ARG1));
  int sys = system(command);
#if HAVE_SYSTEM
  return(unify(ARG2, MkIntTerm(sys)));
#endif
}

/* execute a command as a detached process */
static int
p_wait(void)
{
  Int pid = IntOfTerm(ARG1);
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
  if (mode == 0)
    pfd = popen(command, "r");
  else
    pfd = popen(command, "w");
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
  return(unify(ARG2, MkIntTerm(out)));
}

/* host info */

static int
host_name(void)
{
  char name[256];
#if HAVE_GETHOSTNAME
  if (gethostname(name, 256) == -1) {
    /* return an error number */
    return(unify(ARG2, MkIntTerm(errno)));
  }
#endif
  return(unify(ARG1, MkAtomTerm(LookupAtom(name))));
}

static int
host_id(void)
{
#if HAVE_GETHOSTID
  return(unify(ARG1, MkIntTerm(gethostid())));
#endif
}

static int
pid(void)
{
  return(unify(ARG1, MkIntTerm(getpid())));
}

static int
p_kill(void)
{
#if HAVE_KILL
  if (kill(IntOfTerm(ARG1), IntOfTerm(ARG2)) < 0) {
    /* return an error number */
    return(unify(ARG2, MkIntTerm(errno)));
  }
#else
 oops
#endif
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
  UserCPredicate("file_property", file_property, 6);
  UserCPredicate("unlink", p_unlink, 2);
  UserCPredicate("mkdir", p_mkdir, 2);
  UserCPredicate("rmdir", p_rmdir, 2);
  UserCPredicate("dir_separator", dir_separator, 1);
  UserCPredicate("p_environ", p_environ, 2);
  UserCPredicate("exec_command", execute_command, 6);
  UserCPredicate("do_shell", shell, 2);
  UserCPredicate("do_system", p_system, 2);
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
}

#ifdef _WIN32

#include <windows.h>

int WINAPI PROTO(win_system, (HANDLE, DWORD, LPVOID));

int WINAPI win_system(HANDLE hinst, DWORD reason, LPVOID reserved)
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
p  return 1;
}
#endif
