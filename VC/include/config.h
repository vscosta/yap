/* config.h.  Generated automatically by configure.  */

/* are we using gcc */
/* #define HAVE_GCC 1 */

/* should we use gcc threaded code (i.e. goto *adrs) */
/* #define USE_THREADED_CODE 1*/

/* Should we use lib readline ? 	*/
/* #undef HAVE_LIBREADLINE */

/* Should we use gmp ? 	*/
/* #define HAVE_LIBGMP 1 */

/* does the compiler support inline ? */
/* #undef inline */

/* Do we have Ansi headers ?		*/
#define STDC_HEADERS 1

/* Host Name ?				*/
#define HOST_ALIAS ""

/* #undef HAVE_SYS_WAIT_H */
#define NO_UNION_WAIT 1

/* #undef  HAVE_ARPA_INET_H */
#define  HAVE_CTYPE_H 1
#define  HAVE_DIRECT_H 1
#define  HAVE_DIRENT_H 1
#define  HAVE_ERRNO_H 1
#define  HAVE_FCNTL_H 1
/* #undef  HAVE_FENV_H */
/* #undef  HAVE_FPU_CONTROL_H */
/* #undef  HAVE_GMP_H */
/* #undef  HAVE_IEEEFP_H */
#define  HAVE_IO_H 1
#define  HAVE_LIMITS_H 1
#define  HAVE_MEMORY_H 1
/* #undef  HAVE_NETDB_H */
/* #undef  HAVE_NETINET_IN_H */
/* #undef  HAVE_READLINE_READLINE_H */
/* #undef  HAVE_REGEX_H */
/* #undef  HAVE_SIGINFO_H */
#define  HAVE_SIGNAL_H 1
#define  HAVE_STDARG_H 1
#define  HAVE_STRING_H 1
/* #undef  HAVE_STROPTS_H */
/* #undef  HAVE_SYS_CONF_H */
#define  HAVE_SYS_FILE_H 1
/* #undef  HAVE_SYS_MMAN_H */
/* #undef  HAVE_SYS_PARAM_H */
/* #undef  HAVE_SYS_RESOURCE_H */
/* #undef  HAVE_SYS_SELECT_H */
/* #undef  HAVE_SYS_SHM_H */
/* #undef  HAVE_SYS_SOCKET_H */
#define  HAVE_SYS_STAT_H 1
#define  HAVE_SYS_TIME_H 1
/* #undef  HAVE_SYS_TIMES_H */
#define  HAVE_SYS_TYPES_H 1
/* #undef  HAVE_SYS_UCONTEXT_H */
/* #undef  HAVE_SYS_UN_H */
#define  HAVE_TIME_H 1
#define  HAVE_UNISTD_H 1
#define  HAVE_WINSOCK_H 1
#define  HAVE_WINSOCK2_H 1

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

/* Set the minimum and default heap, trail and stack size */
#define MinTrailSpace ( 32*SIZEOF_INT_P)
#define MinStackSpace (200*SIZEOF_INT_P)
#define  MinHeapSpace (200*SIZEOF_INT_P)

#define UsrTrailSpace (0)
#define UsrStackSpace (0)
#define  UsrHeapSpace (0)

#if (UsrTrailSpace > MinTrailSpace)
  #define DefTrailSpace	UsrTrailSpace
#else
  #define DefTrailSpace	MinTrailSpace
#endif

#if (UsrStackSpace > MinStackSpace)
  #define DefStackSpace	UsrStackSpace
#else
  #define DefStackSpace	MinStackSpace
#endif

#if (UsrHeapSpace > MinHeapSpace)
  #define DefHeapSpace	UsrHeapSpace
#else
  #define DefHeapSpace	MinHeapSpace
#endif



/* Define return type for signal	*/
#define  RETSIGTYPE void

/* #undef  HAVE_ACOSH */
/* #undef  HAVE_ALARM */
/* #undef  HAVE_ASINH */
/* #undef  HAVE_ATANH */
#define  HAVE_CHDIR 1
#define  HAVE_DUP2 1
/* #undef  HAVE_FETESTEXCEPT */
/* #undef  HAVE_FINITE */
#define  HAVE_GETCWD 1
#define  HAVE_GETENV 1
/* #undef  HAVE_GETHOSTBYNAME */
/* #undef  HAVE_GETHOSTID */
/* #undef  HAVE_GETHOSTNAME */
/* #undef  HAVE_GETHRTIME */
/* #undef  HAVE_GETPWNAM */
/* #undef  HAVE_GETRUSAGE */
/* #undef  HAVE_GETTIMEOFDAY */
/* #undef  HAVE_GETWD */
#define  HAVE_ISATTY 1
/* #undef  HAVE_ISNAN */
/* #undef  HAVE_KILL */
#define  HAVE_LABS 1
/* #undef  HAVE_LINK */
#define  HAVE_LOCALTIME 1
/* #undef  HAVE_LSTAT */
#define  HAVE_MEMCPY 1
#define  HAVE_MEMMOVE 1
/* #undef  HAVE_MKSTEMP */
#define  HAVE_MKTEMP 1
/* #undef  HAVE_MMAP */
#define  HAVE_OPENDIR 1
#define  HAVE_POPEN 1
#define  HAVE_PUTENV 1
#define  HAVE_RAND 1
/* #undef  HAVE_RANDOM */
#define  HAVE_RENAME 1
/* #undef  HAVE_RINT */
/* #undef  HAVE_RL_SET_PROMPT */
/* #undef  HAVE_SBRK */
/* #undef  HAVE_SELECT */
#define  HAVE_SETBUF 1
/* #undef  HAVE_SETLINEBUF */
/* #undef  HAVE_SHMAT */
/* #undef  HAVE_SIGACTION */
/* #undef  HAVE_SIGGETMASK */
/* #undef  HAVE_SIGINTERRUPT */
#define  HAVE_SIGNAL 1
/* #undef  HAVE_SIGPROCMASK */
#define  HAVE_SIGSETJMP 0
#define  HAVE_SLEEP 1
/* #undef  HAVE_SNPRINTF */
/* #undef  HAVE_SOCKET */
#define  HAVE_STAT 1
#define  HAVE_STRCHR 1
#define  HAVE_STRERROR 1
#define  HAVE_STRNCAT 1
#define  HAVE_STRNCPY 1
#define  HAVE_STRTOD 1
#define  HAVE_SYSTEM 1
#define  HAVE_TIME 1
/* #undef  HAVE_TIMES */
#define  HAVE_TMPNAM 1
/* #undef  HAVE_USLEEP */
/* #undef  HAVE_VSNPRINTF */
/* #undef  HAVE_WAITPID */
#define  HAVE_MPZ_XOR 0

#define  HAVE_SIGSEGV 1

#define  HAVE_ENVIRON 1

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

/* Is fflush(NULL) clobbering input streams? */
#define BROKEN_FFLUSH_NULL 1
