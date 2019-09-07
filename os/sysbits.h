// @{

/**
   @addtogroup YAPOS
*/

/*
 * In this routine we shall try to include the inevitably machine dependant
 * routines. These include, for the moment : Time, A rudimentary form of
 * signal handling, OS calls,
 *
 * Vitor Santos Costa, February 1987
 *
 */

#include "YapConfig.h"

#if _WIN32 || defined(__MINGW32__)
#if !defined(MINGW_HAS_SECURE_API)
#define MINGW_HAS_SECURE_API 1
#endif
//#undef _POSIX_
#endif
#include "Yap.h"
#include "YapEval.h"
#include "YapHeap.h"
#include "YapText.h"
#include "Yatom.h"
#include "yapio.h"

// Win32 InputOutput Support
#if _WIN32 || defined(__MINGW32__)
#include <winsock2.h>
/* Windows */
#include "shlwapi.h"
#include <direct.h>
#include <io.h>
#include <windows.h>
#ifndef S_ISDIR
#define S_ISDIR(x) (((x)&_S_IFDIR) == _S_IFDIR)
#endif
#endif
#ifndef BUF_SIZE
#ifdef MAX_PATH
#define BUF_SIZE MAX_PATH
#endif
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#include <stdlib.h>

#if HAVE_SYS_PARAM_Hb
#include <sys/param.h>
#endif

#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#if HAVE_CTYPE_H
#include <ctype.h>
#endif
#if HAVE_SYS_PARAMS_H
#include <sys/params.h>
#endif
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if HAVE_SYS_SELECT_H && !_MSC_VER && !defined(__MINGW32__)
#include <sys/select.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_LIBGEN_H
#include <libgen.h>
#endif
#if HAVE_WCTYPE_H
#include <wctype.h>
#endif
#if HAVE_LIMITS_H
#include <limits.h>
#endif
#if HAVE_ERRNO_H
#include <errno.h>
#endif
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if !HAVE_STRNCAT
#define strncat(X, Y, Z) strcat(X, Y)
#endif
#if !HAVE_STRNCPY
#define strncpy(X, Y, Z) strcpy(X, Y)
#endif
#include "iopreds.h"

#if HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
/* CYGWIN seems to include this automatically */
#if HAVE_FENV_H // && !defined(__CYGWIN__)
#include <fenv.h>
#endif

#ifdef MPW
#define signal sigset
#endif

/* windows.h does not like absmi.h, this
   should fix it for now */
#include <math.h>
#if HAVE_TIME_H
#include <time.h>
#endif
#if HAVE_SYS_TIME_H && !_MSC_VER
#include <sys/time.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_SYS_WAIT_H && !defined(__MINGW32__) && !_MSC_VER
#include <sys/wait.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_GETPWNAM
#include <pwd.h>
#endif
#include <ctype.h>
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if HAVE_WORDEXP_H
#include <wordexp.h>
#endif
#if HAVE_GLOB_H
#include <glob.h>
#endif
#if HAVE_LIBGEN_H
#include <libgen.h>
#endif
#if defined(HAVE_READLINE_READLINE_H)
#include <readline/readline.h>
#endif

#if _MSC_VER || defined(__MINGW32__)
#define SYSTEM_STAT _stat
#else
#define SYSTEM_STAT stat
#endif

extern void Yap_InitRandom(void);
extern void Yap_InitTime(int wid);
extern void Yap_InitOSSignals(int wid);
extern void Yap_InitWTime(void);

static inline char *OsPath(const char *p, char *buf) { return (char *)p; }

static inline char *PrologPath(const char *Y, char *X) { return (char *)Y; }


/// File Error Handler
static inline void FileError(yap_error_number type, Term where, const char *format,
                      ...) {

    if (trueLocalPrologFlag(FILEERRORS_FLAG)) {
        va_list ap;

        va_start(ap, format);
        /* now build the error string */
        Yap_Error(type, TermNil, format, ap);
        va_end(ap);
    }
}


#define isValidEnvChar(C)                                                      \
  (((C) >= 'a' && (C) <= 'z') || ((C) >= 'A' && (C) <= 'Z') || (C) == '_')

