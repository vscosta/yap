/* config.h.  Generated automatically by configure.  */

/* are we using gcc */
#define HAVE_GCC 1

/* should we use gcc threaded code (i.e. goto *adrs) */
#define USE_THREADED_CODE 1

/* Should we use lib readline ? 	*/
/* #undef HAVE_LIBREADLINE */

/* Should we use gmp ? 	*/
/* #undef HAVE_LIBGMP */

/* does the compiler support inline ? */
/* #undef inline */

/* Do we have Ansi headers ?		*/
#define STDC_HEADERS 1

/* Host Name ?				*/
#define HOST_ALIAS "i386-pc-cygwin32"

/* #undef HAVE_SYS_WAIT_H */
#define NO_UNION_WAIT 1

#define  HAVE_ARPA_INET_H 1
#define  HAVE_CTYPE_H 1
#define  HAVE_DIRECT_H 1
#define  HAVE_ERRNO_H 1
#define  HAVE_FCNTL_H 1
/* #undef  HAVE_FENV_H */
/* #undef  HAVE_FPU_CONTROL_H */
#define  HAVE_IEEEFP_H 1
#define  HAVE_LIMITS_H 1
#define  HAVE_MEMORY_H 1
#define  HAVE_NETDB_H 1
#define  HAVE_NETINET_IN_H 1
/* #undef  HAVE_REGEX_H */
/* #undef  HAVE_SIGINFO_H */
#define  HAVE_STDARG_H 1
#define  HAVE_STRING_H 1
#define  HAVE_SYS_FILE_H 1
#define  HAVE_SYS_MMAN_H 1
#define  HAVE_SYS_PARAM_H 1
#define  HAVE_SYS_RESOURCE_H 1
#define  HAVE_SYS_SELECT_H 1
/* #undef  HAVE_SYS_SHM_H */
#define  HAVE_SYS_SOCKET_H 1
#define  HAVE_SYS_STAT_H 1
#define  HAVE_SYS_TIME_H 1
#define  HAVE_SYS_TIMES_H 1
#define  HAVE_SYS_TYPES_H 1
/* #undef  HAVE_SYS_UCONTEXT_H */
#define  HAVE_SYS_UN_H 1
#define  HAVE_TIME_H 1
#define  HAVE_UNISTD_H 1
#define  HAVE_WINSOCK_H 1
#define  HAVE_WINSOCK2_H 1
/* #undef  HAVE_GMP_H */

/* Do we have restartable syscalls */
/* #undef  HAVE_RESTARTABLE_SYSCALLS */

/* is 'tms' defined in <sys/time.h> ? */
/* #undef  TM_IN_SYS_TIME */

/* define type of prt returned by malloc: char or void */
#define  MALLOC_T void *

/* Define byte order			*/
/* #undef  WORDS_BIGENDIAN */

/* Define sizes of some basic types	*/
#define  SIZEOF_INT_P 4
#define  SIZEOF_INT 4
#define  SIZEOF_SHORT_INT 2
#define  SIZEOF_LONG_INT 4
#define  SIZEOF_LONG_LONG_INT 8
#define  SIZEOF_FLOAT 4
#define  SIZEOF_DOUBLE 8

/* Define representation of floats      */
/* only one of the following shoud be set */
/* to add a new representation you must edit FloatOfTerm and MkFloatTerm
  in adtdefs.c
*/
#define  FFIEEE 1
/* #undef   FFVAX */   		/* manual */

/* Define the standard type of a float argument to a function */
#define  FAFloat double		/* manual */

/* Define return type for signal	*/
#define  RETSIGTYPE void

/* #undef  HAVE_ALARM */
/* #undef  HAVE_ASINH */
/* #undef  HAVE_ACOSH */
/* #undef  HAVE_ATANH */
#define  HAVE_CHDIR 1
#define  HAVE_DUP2 1
/* #undef  HAVE_FETESTEXCEPT */
/* #undef  HAVE_FINITE */
/* #undef  HAVE_GETRUSAGE */
#define  HAVE_GETCWD 1
#define  HAVE_GETENV 1
/* #undef  HAVE_GETHRTIME */
/* #undef  HAVE_GETPWNAM */
/* #undef  HAVE_GETTIMEOFDAY */
/* #undef  HAVE_GETWD */
#define  HAVE_ISATTY 1
/* #undef  HAVE_ISNAN */
#define  HAVE_LABS 1
/* #undef  HAVE_LINK */
/* #undef  HAVE_MMAP */
#define  HAVE_MEMCPY 1
#define  HAVE_MEMMOVE 1
/* #undef  HAVE_MKSTEMP */
#define  HAVE_PUTENV 1
#define  HAVE_RAND 1
/* #undef  HAVE_RANDOM */
/* #undef  HAVE_RINT */
/* #undef  HAVE_SBRK */
#define  HAVE_STAT 1
/* #undef  HAVE_SELECT */
#define  HAVE_SETBUF 1
/* #undef  HAVE_SHMAT */
/* #undef  HAVE_SIGACTION */
/* #undef  HAVE_SIGGETMASK */
#define  HAVE_SIGNAL 1
/* #undef  HAVE_SIGPROCMASK */
#define  HAVE_SIGSEGV 1
#define  HAVE_SIGSETJMP 0
/* #undef  HAVE_SNPRINTF */
/* #undef  HAVE_SOCKET */
#define  HAVE_STRERROR 1
#define  HAVE_STRNCAT 1
#define  HAVE_STRNCPY 1
#define  HAVE_STRCHR 1
#define  HAVE_STRTOD 1
#define  HAVE_SYSTEM 1
/* #undef  HAVE_TIMES */
#define  HAVE_TMPNAM 1
/* #undef  HAVE_VSNPRINTF */
#define  HAVE_ENVIRON 1
#define  HAVE_MPZ_XOR 0

#define  SELECT_TYPE_ARG1    
#define  SELECT_TYPE_ARG234  
#define  SELECT_TYPE_ARG5    

#define  TYPE_SELECT_
#define  MYTYPE(X) MYTYPE1#X

/* define how to pass the address of a function */
#define FunAdr(Fn)  Fn

#define  ALIGN_LONGS 1
#define  LOW_ABSMI 0

#define  MSHIFTOFFS 1

#define USE_MMAP    (HAVE_MMAP)
#define USE_SHM	    (HAVE_SHMAT & !HAVE_MMAP)
#define USE_SBRK    (HAVE_SBRK & !HAVE_MMAP & !HAVE_SHMAT)

/* for OSes that do not allow user access to the first
   quadrant of the memory space */
/* #undef FORCE_SECOND_QUADRANT */

#if (HAVE_SOCKET || defined(__MINGW32__)) && !defined(SIMICS)
#define USE_SOCKET 1
#endif

#if HAVE_GMP_H && HAVE_LIBGMP
#define USE_GMP 1
#endif

