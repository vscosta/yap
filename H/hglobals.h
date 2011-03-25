
  /* This file, hglobals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/GLOBALS instead */



















typedef struct global_data {

#if THREADS

  UInt  n_of_threads;

  UInt  n_of_threads_created;

  UInt  threads_total_time;
#endif

#if defined(YAPOR) || defined(THREADS)

  lockvar  bgl;
#endif

  int  allow_local_expansion;
  int  allow_global_expansion;
  int  allow_trail_expansion;
  UInt  size_of_overflow;

  UInt  agc_last_call;

  UInt  agc_threshold;
  Agc_hook  agc_hook;

#if HAVE_LIBREADLINE
  char  *readline_buf;
  char  *readline_pos;
#endif

#ifdef THREADS
  lockvar  thread_handles_lock;
#endif 

#if defined(YAPOR) || defined(TABLING)
  struct optyap_global_data  optyap_global;
  struct local_data  remote[MAX_WORKERS];
#endif

  int  initialised;
  int  initialised_from_pl;
  int  pl_argc;
  char  **pl_argv;

  struct halt_hook  *yap_halt_hook;
} w_shared;
