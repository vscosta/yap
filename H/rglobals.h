
  /* This file, rglobals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/GLOBALS instead */


















static void RestoreGlobal(void) {
















#if THREADS






#endif

#if defined(YAPOR) || defined(THREADS)

  REINIT_LOCK(GLOBAL_BGL);
#endif

#ifdef THREADS
  REINIT_LOCK(GLOBAL_ThreadHandlesLock);
#endif 
#if defined(YAPOR) || defined(TABLING)

#endif /* YAPOR || TABLING */
}
