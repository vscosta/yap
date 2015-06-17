
#ifndef PL_GLOBAL_H

#define PL_GLOBAL_H

#include <setjmp.h>

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


typedef struct find_data_tag *  FindData;       /* pl-trace.c */

typedef enum
{ LDATA_IDLE = 0,
  LDATA_SIGNALLED,
  LDATA_ANSWERING,
  LDATA_ANSWERED
} ldata_status_t;

typedef enum
{ CLN_NORMAL = 0,			/* Normal mode */
  CLN_ACTIVE,				/* Started cleanup */
  CLN_FOREIGN,				/* Foreign hooks */
  CLN_PROLOG,				/* Prolog hooks */
  CLN_SHARED,				/* Unload shared objects */
  CLN_DATA				/* Remaining data */
} cleanup_status;

#ifdef THREADS

typedef struct free_chunk *FreeChunk;   /* left-over chunk */

struct free_chunk
{ FreeChunk     next;                   /* next of chain */
  size_t        size;                   /* size of free bit */
};

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
intptr_t system_thread_id(void);

#endif

typedef struct
{ unsigned long flags;                  /* Fast access to some boolean Prolog flags */
} pl_features_t;

typedef enum
{ OCCURS_CHECK_FALSE = 0,
  OCCURS_CHECK_TRUE,
  OCCURS_CHECK_ERROR
} occurs_check_t;

typedef enum
{ ACCESS_LEVEL_USER = 0,        /* Default user view */
  ACCESS_LEVEL_SYSTEM           /* Allow low-level access */
} access_level_t;

typedef struct exception_frame		/* PL_throw exception environments */
{ struct exception_frame *parent;	/* parent frame */
  jmp_buf	exception_jmp_env;	/* longjmp environment */
} exception_frame;

typedef struct
{ atom_t	file;			/* current source file */
  IOPOS         position;               /* Line, line pos, char and byte */
} source_location;

typedef struct
{ size_t        localSize;              /* size of local stack */
  size_t        globalSize;             /* size of global stack */
  size_t        trailSize;              /* size of trail stack */
  char *        goal;                   /* initial goal */
  char *        topLevel;               /* toplevel goal */
  char *        initFile;               /* -f initialisation file */
  char *        systemInitFile;         /* -F initialisation file */
  //  opt_list     *scriptFiles;
  // opt_list     *search_paths;           /* -p path */
  char *        pldoc_server;           /* --pldoc=Server */
  char *        compileOut;             /* file to store compiler output */
  char *        saveclass;              /* Type of saved state */
  bool          silent;                 /* -q: quiet operation */
#ifdef __WINDOWS__
  bool          win_app;                /* --win_app: be Windows application */
#endif
} pl_options_t;


/* The GD global variable */
typedef struct {
  pl_options_t  options;                /* command-line options */
  int io_initialised;
  cleanup_status cleaning;		/* Inside PL_cleanup() */

  pl_defaults_t	defaults;		/* system default settings */

 struct
  { Table       table;                  /* global (read-only) features */
  } prolog_flag;

  struct
  { Table		tmp_files;	/* Known temporary files */
    CanonicalDir	_canonical_dirlist;
    char *		myhome;		/* expansion of ~ */
    char *		fred;		/* last expanded ~user */
    char *		fredshome;	/* home of fred */
    OnHalt		on_halt_list;	/* list of onhalt hooks */
    int			halting;	/* process is shutting down */
    int			gui_app;	/* Win32: Application is a gui app */
    IOFUNCTIONS		iofunctions;	/* initial IO functions */
    IOFUNCTIONS 	org_terminal;	/* IO+Prolog terminal functions */
    IOFUNCTIONS		rl_functions;	/* IO+Terminal+Readline functions */
  } os;

  struct
  { size_t	heap;			/* heap in use */
    size_t	atoms;			/* No. of atoms defined */
    size_t	atomspace;		/* # bytes used to store atoms */
    size_t	stack_space;		/* # bytes on stacks */
#ifdef O_ATOMGC
    size_t	atomspacefreed;		/* Freed atom-space */
#endif
    int		functors;		/* No. of functors defined */
    int		predicates;		/* No. of predicates defined */
    int		modules;		/* No. of modules in the system */
    intptr_t	codes;			/* No. of byte codes generated */
#ifdef O_PLMT
    int		threads_created;	/* # threads created */
    int		threads_finished;	/* # finished threads */
    double	thread_cputime;		/* Total CPU time of threads */
#endif
    double	start_time;		/* When Prolog was started */
  } statistics;

  struct
  { atom_t *	array;			/* index --> atom */
    size_t	count;			/* elements in array */
    atom_t     *for_code[256];		/* code --> one-char-atom */
  } atoms;

  struct
  { int         os_argc;                /* main(int argc, char **argv) */
    char **     os_argv;
    int         appl_argc;              /* Application options */
    char **     appl_argv;
    int         notty;                  /* -tty: donot use ioctl() */
    int		optimise;		/* -O: optimised compilation */
  } cmdline;

#if 0
  struct
  { //char *      CWDdir;
    //size_t      CWDlen;
    //char *      executable;             /* Running executable */
#ifdef __WINDOWS__
    char *      module;                 /* argv[0] module passed */
#endif
  } paths;
#endif

  struct
  { ExtensionCell _ext_head;		/* head of registered extensions */
    ExtensionCell _ext_tail;		/* tail of this chain */

    InitialiseHandle initialise_head;	/* PL_initialise_hook() */
    InitialiseHandle initialise_tail;
    PL_dispatch_hook_t dispatch_events; /* PL_dispatch_hook() */

    int		  _loaded;		/* system extensions are loaded */
  } foreign;

#ifdef O_PLMT
  FreeChunk	    left_over_pool;	/* Left-over from threads */

  struct
  { struct _at_exit_goal *exit_goals;	/* Global thread_at_exit/1 goals */
    int		    	enabled;	/* threads are enabled */
    Table		mutexTable;	/* Name --> mutex table */
    int			mutex_next_id;	/* next id for anonymous mutexes */
    struct pl_mutex*	MUTEX_load;	/* The $load mutex */
#ifdef __WINDOWS__
    HINSTANCE	    	instance;	/* Win32 process instance */
#endif
    counting_mutex     *mutexes;	/* Registered mutexes */
    int			thread_max;	/* Maximum # threads */
    PL_thread_info_t  *threads[MAX_THREADS];	/* Pointers to thread-info */
  } thread;
#endif /*O_PLMT*/

  struct				/* pl-format.c */
  { Table	predicates;
  } format;

  struct
  {/*  Procedure	dgarbage_collect1; */
/*     Procedure	catch3; */
/*     Procedure	true0; */
/*     Procedure	fail0; */
/*     Procedure	equals2;		/\* =/2 *\/ */
/*     Procedure	is2;			/\* is/2 *\/ */
/*     Procedure	strict_equal2;		/\* ==/2 *\/ */
/*     Procedure	event_hook1; */
/*     Procedure	exception_hook4; */
/*     Procedure	print_message2; */
/*     Procedure	foreign_registered2;	/\* $foreign_registered/2 *\/ */
/*     Procedure	prolog_trace_interception4; */
    predicate_t	portray;		/* portray/1 */
/*     Procedure   dcall1;			/\* $call/1 *\/ */
/*     Procedure	setup_call_catcher_cleanup4; /\* setup_call_catcher_cleanup/4 *\/ */
/*     Procedure	undefinterc4;		/\* $undefined_procedure/4 *\/ */
/*     Procedure   dthread_init0;		/\* $thread_init/0 *\/ */
/*     Procedure   dc_call_prolog0;	/\* $c_call_prolog/0 *\/ */
/* #ifdef O_ATTVAR */
/*     Procedure	dwakeup1;		/\* system:$wakeup/1 *\/ */
    predicate_t	portray_attvar1;	/* $attvar:portray_attvar/1 */
/* #endif */
/* #ifdef O_CALL_RESIDUE */
/*     Procedure	call_residue_vars2;	/\* $attvar:call_residue_vars/2 *\/ */
/* #endif */

/*     SourceFile  reloading;		/\* source file we are re-loading *\/ */
/*     int		active_marked;		/\* #prodedures marked active *\/ */
/*     int		static_dirty;		/\* #static dirty procedures *\/ */

/* #ifdef O_CLAUSEGC */
/*     DefinitionChain dirty;		/\* List of dirty static procedures *\/ */
/* #endif */
  } procedures;

#ifdef O_LOCALE
  struct
  { Table               localeTable;    /* Name --> locale table */
    PL_locale          *default_locale; /* System wide default */
  } locale;
#endif

} gds_t;

extern gds_t gds;

#define GD (&gds)

/* The LD macro layer */
typedef struct PL_local_data {

  struct				/* Local IO stuff */
  { IOSTREAM *streams[6];		/* handles for standard streams */
    struct input_context *input_stack;	/* maintain input stream info */
    struct output_context *output_stack; /* maintain output stream info */
    st_check stream_type_check;		/* Check bin/text streams? */
  } IO;

  struct
  { Table	  table;		/* Feature table */
    pl_features_t mask;			/* Masked access to booleans */
    int		  write_attributes;	/* how to write attvars? */
    occurs_check_t occurs_check;	/* Unify and occurs check */
  } feature;


  source_location read_source;		/* file, line, char of last term */

  term_t read_varnames;          /* varnames of last term */

  struct
  { int		active;			/* doing pipe I/O */
    jmp_buf	context;		/* context of longjmp() */
  } pipe;

  struct
  { atom_t	current;		/* current global prompt */
    atom_t	first;			/* how to prompt first line */
    int		first_used;		/* did we do the first line? */
    int		next;			/* prompt on next read operation */
  } prompt;

  struct
  { Table         table;                /* Feature table */
    pl_features_t mask;                 /* Masked access to booleans */
    int           write_attributes;     /* how to write attvars? */
    occurs_check_t occurs_check;        /* Unify and occurs check */
    access_level_t access_level;        /* Current access level */
  } prolog_flag;

  int           break_level;            /* break */
  void *        glob_info;              /* pl-glob.c */
  IOENC		encoding;		/* default I/O encoding */

  struct
  { char *	_CWDdir;
    size_t	_CWDlen;
#ifdef __BEOS__
    status_t	dl_error;		/* dlopen() emulation in pl-beos.c */
#endif
    int		rand_initialised;	/* have we initialised random? */
  } os;

 struct
  { int64_t     pending;                /* PL_raise() pending signals */
    int         current;                /* currently processing signal */
    int         is_sync;                /* current signal is synchronous */
    record_t    exception;              /* Pending exception from signal */
#ifdef O_PLMT
    simpleMutex sig_lock;               /* lock delivery and processing */
#endif
  } signal;

  int		critical;		/* heap is being modified */

  struct
  { term_t	term;			/* exception term */
    term_t	bin;			/* temporary handle for exception */
    term_t	printed;		/* already printed exception */
    term_t	tmp;			/* tmp for errors */
    term_t	pending;		/* used by the debugger */
    int		in_hook;		/* inside exception_hook() */
    int		processing;		/* processing an exception */
    exception_frame *throw_environment;	/* PL_throw() environments */
  } exception;
  const char   *float_format;		/* floating point format */

  struct
  { FindData    find;                   /* /<ports> <goal> in tracer */
  } trace;

  pl_debugstatus_t _debugstatus;        /* status of the debugger */

#ifdef O_PLMT
  struct
  { //intptr_t   magic;			/* PL_THREAD_MAGIC (checking) */
    struct _PL_thread_info_t *info;	/* info structure */
    //unsigned forall_flags;		/* forThreadLocalData() flags */
					/* Communication */
    //message_queue messages;		/* Message queue */
    //struct _thread_sig   *sig_head;	/* Head of signal queue */
    //struct _thread_sig   *sig_tail;	/* Tail of signal queue */
    //struct _at_exit_goal *exit_goals;	/* thread_at_exit/1 goals */
    //DefinitionChain local_definitions;	/* P_THREAD_LOCAL predicates */
  } thread;
#endif

  struct {
    buffer	_discardable_buffer;	/* PL_*() character buffers */
    buffer	_buffer_ring[BUFFER_RING_SIZE];
    int		_current_buffer_id;
  } fli;

  struct
  { fid_t	numbervars_frame;	/* Numbervars choice-point */
  } var_names;

#ifdef O_GMP
  struct
  {
    int		persistent;		/* do persistent operations */
  } gmp;
#endif

  int in_print_message;

  struct regstore_t *reg_cache;         /* pointer to YAP registers */

#ifdef O_LOCALE
  struct
  { PL_locale *current;                 /* Current locale */
  } locale;
#endif

}  PL_local_data_t;


extern PL_local_data_t lds;

#endif
