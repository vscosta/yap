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

/* windows.h does not like absmi.h, this
   should fix it for now */
#if _WIN32 || __MINGW32__
#include <winsock2.h>
#endif
#include "absmi.h"
#include "yapio.h"
#include "iopreds.h"
#include "alloc.h"
#include <math.h>
#if STDC_HEADERS
#include <stdlib.h>
#endif
#if HAVE_WINDOWS_H
#include <windows.h>
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
#if !HAVE_STRNCAT
#define strncat(X,Y,Z) strcat(X,Y)
#endif
#if !HAVE_STRNCPY
#define strncpy(X,Y,Z) strcpy(X,Y)
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
#if  _MSC_VER || defined(__MINGW32__)
#include <windows.h>
/* required for DLL compatibility */
#if HAVE_DIRECT_H
#include <direct.h>
#endif
#include <io.h>
#include <shlwapi.h>
#else
#if HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#endif
/* CYGWIN seems to include this automatically */
#if HAVE_FENV_H && !defined(__CYGWIN__)
#include <fenv.h>
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
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#if HAVE_ERRNO_H
#include <errno.h>
#endif
#if HAVE_READLINE_READLINE_H
#include <readline/readline.h>
#endif

void Yap_InitRandom (void);
void Yap_InitTime (int wid);
void Yap_InitOSSignals (int wid);
void Yap_InitWTime(void);
void Yap_InitLastWTime ( void );


