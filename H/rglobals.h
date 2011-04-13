
  /* This file, rglobals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/GLOBALS instead */



















static void RestoreGlobal(void) {

#if THREADS






#endif

#if defined(YAPOR) || defined(THREADS)

  REINIT_LOCK(Yap_global->bgl);
#endif











#ifdef THREADS
  REINIT_LOCK(Yap_global->thread_handles_lock);
#endif 

#if defined(YAPOR) || defined(TABLING)


#endif







}
