
  /* This file, iglobals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/GLOBALS instead */



















static void InitGlobal(void) {

#if THREADS

  Yap_global->n_of_threads = 1;

  Yap_global->n_of_threads_created = 1;

  Yap_global->threads_total_time = 0L;
#endif

#if defined(YAPOR) || defined(THREADS)

  INIT_LOCK(Yap_global->bgl);
#endif

  Yap_global->allow_local_expansion = TRUE;
  Yap_global->allow_global_expansion = TRUE;
  Yap_global->allow_trail_expansion = TRUE;
  Yap_global->size_of_overflow = 0;

  Yap_global->agc_last_call = 0;

  Yap_global->agc_threshold = 10000;
  Yap_global->agc_hook = NULL;

#ifdef THREADS
  INIT_LOCK(Yap_global->thread_handles_lock);
#endif 

#if defined(YAPOR) || defined(TABLING)


#endif

  Yap_global->initialised = FALSE;
  Yap_global->initialised_from_pl = FALSE;
  Yap_global->pl_argc = 0;
  Yap_global->pl_argv = NULL;

  Yap_global->yap_halt_hook = NULL;
}
