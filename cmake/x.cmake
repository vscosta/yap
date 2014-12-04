#include <dirent.h>
check_symbol_exists( opendir HAVE_OPENDIR )


# locale.h
check_symbol_exists( localeconv HAVE_LOCALECONV )
check_symbol_exists( setlocale HAVE_SETLOCALE )

#include <mach-o/dyld.h>
check_symbol_exists( NSLinkModule HAVE_NSLINKMODULE )

#include <malloc.h>
check_symbol_exists( mallinfo HAVE_MALLINFO )

#math.h
check_symbol_exists( acosh math.h HAVE_ACOSH )
check_symbol_exists( asinh math.h HAVE_ASINH )
check_symbol_exists( atanh math.h HAVE_ATANH )
check_symbol_exists( erf math.h  HAVE_ERF )
check_symbol_exists( finite math.h HAVE_FINITE )
check_symbol_exists( ftruncate HAVE_FTRUNCATE )
check_symbol_exists( isfinite  math.h HAVE_ISFINITE )
check_symbol_exists( lgamma math.h HAVE_LGAMMA )
check_symbol_exists( rint  math.h AVE_RINT )

#include <netdb.h>
check_symbol_exists( h_errno HAVE_H_ERRNO )
check_symbol_exists( socklen_t HAVE_SOCKLEN_T ))


#include <regex.h>
check_symbol_exists( regexec HAVE_REGEXEC )

#include <setjmp.h>
check_symbol_exists( sigsetjmp HAVE_SIGSETJMP )
check_symbol_exists( __sigsetjmp HAVE__SIGSETJMP )
if(HAVE___SIGSETJMP)
  set(HAVE_SIGSETJMP 1)
endif(HAVE___SIGSETJMP)

#include <signal.h>
check_symbol_exists( siggetmask HAVE_SIGGETMASK )
check_symbol_exists( SIGFPE HAVE_SIGFPE )
check_symbol_exists( SIGINFO HAVE_SIGINFO )
check_symbol_exists( SIGPROF HAVE_SIGPROF )
check_symbol_exists( SIGSEGV HAVE_SIGSEGV )

#include <stdbool.h>
check_symbol_exists( _bool HAVE__BOOL )

#include <stdio.h>
check_symbol_exists( popen HAVE_POPEN )
check_symbol_exists( rename HAVE_RENAME )
check_symbol_exists( setbuf HAVE_SETBUF )
check_symbol_exists( setlinebuf HAVE_SETLINEBUF )
check_symbol_exists( snprintf HAVE_SNPRINTF )
check_symbol_exists( tmpnam HAVE_TMPNAM )
check_symbol_exists( vsnprintf HAVE_VSNPRINTF )

#string.h
check_symbol_exists( memcpy HAVE_MEMCPY )
check_symbol_exists( memmove HAVE_MEMMOVE )
check_symbol_exists( strcasestr HAVE_STRCASESTR )
check_symbol_exists( strerror HAVE_STRERROR )
check_symbol_exists( strncat HAVE_STRNCAT )
check_symbol_exists( strncpy HAVE_STRNCPY )
check_symbol_exists( strnlen HAVE_STRNLEN )
check_symbol_exists( strchr HAVE_STRCHR )
check_symbol_exists( strlwr HAVE_STRLWR )

#include strings.h
check_symbol_exists( strncasecmp HAVE_STRNCASECMP )

#include <sys/shm.h>
check_symbol_exists( mmap HAVE_MMAP )

# sys/resource
check_symbol_exists( getrusage sys/resource HAVE_GETRUSAGE )

#include <sys/select.h>
check_symbol_exists( select HAVE_SELECT )

#include <sys/shm.h>
check_symbol_exists( shmat HAVE_SHMAT )

#include <sys/socket.h>
check_symbol_exists( socket HAVE_SOCKET )

# sys/stat.h
check_symbol_exists( lstat HAVE_LSTAT )
check_symbol_exists( stat HAVE_STAT )

#include <sys/time.h>
check_symbol_exists( gettimeofday HAVE_GETTIMEOFDAY )

#include <sys/times.h>
check_symbol_exists( times HAVE_TIMES )

#include <sys/types.h>
check_symbol_exists( ssize_t HAVE_SSIZE_T )

#include <sys/wait.h>
ocheck_symbol_exists( waitpid HAVE_WAITPID )

# time.h
check_symbol_exists( localtime HAVE_LOCALTIME )
check_symbol_exists( nanosleep HAVE_NANOSLEEP )
check_symbol_exists( setitimer HAVE_SETITIMER )
check_symbol_exists( timegm HAVE_TIMEGM )

# unistd.h
check_symbol_exists( access unistd.h HAVE_ACCESS )
check_symbol_exists( alarm unistd.h HAVE_ALARM )
check_symbol_exists( chdir unistd.h HAVE_CHDIR )
check_symbol_exists( crypt unistd.h HAVE_CRYPT )
check_symbol_exists( dup2 unistd.h HAVE_DUP2 )
check_symbol_exists( getcwd unistd.h HAVE_GETCWD )
check_symbol_exists( gethostid unistd.h HAVE_GETHOSTID )
check_symbol_exists( gethostname unistd.h HAVE_GETHOSTNAME )
check_symbol_exists( getpagesize unistd.h HAVE_GETPAGESIZE)
check_symbol_exists( pipe2 unistd.h HAVE_PIPE2 )
check_symbol_exists( sbrk unistd.h HAVE_SBRK )

#include <utime.h>
check_symbol_exists( utime HAVE_UTIME )

#include <wchar.h>
check_symbol_exists( wcsdup HAVE_WCSDUP )
check_symbol_exists( wcsnlen HAVE_WCSNLEN )

#windows.h
check_symbol_exists( LoadLibrary HAVE_LOADLIBRARY )
check_symbol_exists( mbscasecoll HAVE_MBSCASECOLL )
check_symbol_exists( mbscoll HAVE_MBSCOLL )
check_symbol_exists( mbsnrtowcs HAVE_MBSNRTOWCS )
check_symbol_exists( stricmp HAVE_STRICMP )


check_symbol_exists( nullptr HAVE_NULLPTR )
check_symbol_exists( sqllen HAVE_SQLLEN )
check_symbol_exists( sqlulen HAVE_SQLULEN )
check_symbol_exists( struct_time_tm_gmtoff HAVE_STRUCT_TIME_TM_GMTOFF )
check_symbol_exists( var_timezone HAVE_VAR_TIMEZONE )
check_symbol_exists( _chsize_s HAVE__CHSIZE_S )
check_symbol_exists( _NSGetEnviron HAVE__NSGETENVIRON )
check_symbol_exists( __builtin_ffsll HAVE___BUILTIN_FFSLL )


