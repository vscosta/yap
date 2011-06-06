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


/* The GD global variable */
typedef struct {
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
  {
    int		optimise;		/* -O: optimised compilation */
  } cmdline;

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
    PL_thread_info_t  **threads;	/* Pointers to thread-info */
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

} gds_t;

extern gds_t gds;

#define GD (&gds)

/* The LD macro layer */
typedef struct PL_local_data {

  struct				/* Local IO stuff */
  { IOSTREAM *streams[6];		/* handles for standard streams */
    struct input_context *input_stack;	/* maintain input stream info */
    struct output_context *output_stack; /* maintain output stream info */
  } IO;

  struct
  { Table	  table;		/* Feature table */
    pl_features_t mask;			/* Masked access to booleans */
    int		  write_attributes;	/* how to write attvars? */
    occurs_check_t occurs_check;	/* Unify and occurs check */
  } feature;


  source_location read_source;		/* file, line, char of last term */

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
  } prolog_flag;

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

  struct {
    buffer	_discardable_buffer;	/* PL_*() character buffers */
    buffer	_buffer_ring[BUFFER_RING_SIZE];
    int		_current_buffer_id;
  } fli;

#ifdef O_GMP
  struct
  { 
    int		persistent;		/* do persistent operations */
  } gmp;
#endif

}  PL_local_data_t;

#define usedStack(D) 0

#define features		(LD->feature.mask)

extern PL_local_data_t lds;

#define exception_term		(LD->exception.term)

// THIS HAS TO BE ABSTRACTED

#define GLOBAL_LD (LOCAL_PL_local_data_p)

#if !defined(O_PLMT) && !defined(YAPOR)
#define LOCAL_LD (GLOBAL_LD)
#define LD (GLOBAL_LD)
#define ARG1_LD   void
#define ARG_LD
#define GET_LD
#define PRED_LD
#define PASS_LD
#define PASS_LD1 

#else

#define LOCAL_LD (__PL_ld)
#define LD	  LOCAL_LD

#define GET_LD	  CACHE_REGS PL_local_data_t *__PL_ld = GLOBAL_LD;
#define ARG1_LD   PL_local_data_t *__PL_ld

#define ARG_LD    , ARG1_LD
#define PASS_LD1  LD
#define PASS_LD   , LD
#define PRED_LD   GET_LD

#endif

#define Suser_input             (LD->IO.streams[0])
#define Suser_output            (LD->IO.streams[1])
#define Suser_error             (LD->IO.streams[2])
#define Scurin                  (LD->IO.streams[3])
#define Scurout                 (LD->IO.streams[4])
#define Sprotocol               (LD->IO.streams[5])
#define Sdin                    Suser_input             /* not used for now */
#define Sdout                   Suser_output

#define source_line_no		(LD->read_source.line)
#define source_file_name	(LD->read_source.file)
#define source_line_pos		(LD->read_source.linepos)
#define source_char_no		(LD->read_source.character)

