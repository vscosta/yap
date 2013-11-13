
#ifndef PL_SHARED_INCLUDE

#define PL_SHARED_INCLUDE

#ifndef _FLI_H_INCLUDED

typedef	void *record_t;
typedef int bool;
typedef struct mod_entry *module_t;
typedef uintptr_t	atom_t;

#endif

typedef enum
{ ACCESS_LEVEL_USER = 0,        /* Default user view */
  ACCESS_LEVEL_SYSTEM           /* Allow low-level access */
} access_level_t;

typedef enum
{ LDATA_IDLE = 0,
  LDATA_SIGNALLED,
  LDATA_ANSWERING,
  LDATA_ANSWERED
} ldata_status_t;

#if _WIN32
#ifndef THREADS
typedef int pthread_t;
#endif
#endif

#if THREADS

typedef struct _PL_thread_info_t
{ int		    pl_tid;		/* Prolog thread id */
  size_t	    local_size;		/* Stack sizes */
  size_t	    global_size;
  size_t	    trail_size;
  size_t	    stack_size;		/* system (C-) stack */
  int		    (*cancel)(int id);	/* cancel function */
  int		    open_count;		/* for PL_thread_detach_engine() */
  bool		    detached;		/* detached thread */
  int		    status;		/* PL_THREAD_* */
  pthread_t	    tid;		/* Thread identifier */
  int		    has_tid;		/* TRUE: tid = valid */
#ifdef __linux__
  pid_t		    pid;		/* for identifying */
#endif
#ifdef __WINDOWS__
  unsigned long	    w32id;		/* Win32 thread HANDLE */
#endif
  struct PL_local_data  *thread_data;	/* The thread-local data  */
  module_t	    module;		/* Module for starting goal */
  record_t	    goal;		/* Goal to start thread */
  record_t	    return_value;	/* Value (term) returned */
  atom_t	    name;		/* Name of the thread */
  ldata_status_t    ldata_status;	/* status of forThreadLocalData() */
} PL_thread_info_t;

PL_thread_info_t *SWI_thread_info(int tid, PL_thread_info_t *info);
intptr_t system_thread_id(PL_thread_info_t *info);

#endif

/* Flags on module.  Most of these flags are copied to the read context
   in pl-read.c.
*/

#define M_SYSTEM		(0x0001) /* system module */
#define M_CHARESCAPE		(0x0002) /* module */
#define DBLQ_CHARS		(0x0004) /* "ab" --> ['a', 'b'] */
#define DBLQ_ATOM		(0x0008) /* "ab" --> 'ab' */
#define DBLQ_STRING		(0x0010) /* "ab" --> "ab" */
#define DBLQ_MASK		(DBLQ_CHARS|DBLQ_ATOM|DBLQ_STRING)
#define UNKNOWN_FAIL		(0x0020) /* module */
#define UNKNOWN_WARNING		(0x0040) /* module */
#define UNKNOWN_ERROR		(0x0080) /* module */
#define UNKNOWN_MASK		(UNKNOWN_ERROR|UNKNOWN_WARNING|UNKNOWN_FAIL)

extern unsigned int
getUnknownModule(module_t m);

typedef enum
  { DBG_OFF = 0,                          /* no debugging */
    DBG_ON,                               /* switch on in current environment */
    DBG_ALL                               /* switch on globally */
  } debug_type;


typedef struct debuginfo
{ size_t        skiplevel;              /* current skip level */
  bool          tracing;                /* are we tracing? */
  debug_type    debugging;              /* are we debugging? */
  int           leashing;               /* ports we are leashing */
  int           visible;                /* ports that are visible */
  bool          showContext;            /* tracer shows context module */
  int           styleCheck;             /* source style checking */
  int           suspendTrace;           /* tracing is suspended now */
  //LocalFrame    retryFrame;             /* Frame to retry */
} pl_debugstatus_t;

#define debugstatus LOCAL_debugstatus        /* status of the debugger */

#endif /* PL_SHARED_INCLUDE */
