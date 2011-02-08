
  /* This file, rglobals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/GLOBALS instead */



















static void RestoreWorker(int wid) {










































  FOREIGN_WL(wid)->global_arena = TermToGlobalOrAtomAdjust(FOREIGN_WL(wid)->global_arena);










  RestoreBallTerm(wid);







#ifdef  COROUTINING
  FOREIGN_WL(wid)->woken_goals = TermToGlobalAdjust(FOREIGN_WL(wid)->woken_goals);
  FOREIGN_WL(wid)->atts_mutable_list = TermToGlobalAdjust(FOREIGN_WL(wid)->atts_mutable_list);
#endif

  FOREIGN_WL(wid)->gc_generation = TermToGlobalAdjust(FOREIGN_WL(wid)->gc_generation);
  FOREIGN_WL(wid)->gc_phase = TermToGlobalAdjust(FOREIGN_WL(wid)->gc_phase);






#if LOW_LEVEL_TRACER

#endif

#if defined(YAPOR) || defined(THREADS)
  REINIT_LOCK(FOREIGN_WL(wid)->signal_lock);




#if DEBUG && COROUTINING

#endif




#if GC_NO_TAGS

#endif
#if defined(TABLING) || defined(SBA)



#else



#endif











#endif /* defined(YAPOR) || defined(THREADS) */

  FOREIGN_WL(wid)->dynamic_arrays = PtoArrayEAdjust(FOREIGN_WL(wid)->dynamic_arrays);
  FOREIGN_WL(wid)->static_arrays = PtoArraySAdjust(FOREIGN_WL(wid)->static_arrays);
  FOREIGN_WL(wid)->global_variables = PtoGlobalEAdjust(FOREIGN_WL(wid)->global_variables);

















#if (defined(YAPOR) || defined(TABLING)) && defined(THREADS)

#endif
#ifdef THREADS

#define FOREIGN_ThreadHandle(wid)  (Yap_WLocal[(wid)].thread_handle)		       						
#define MY_ThreadHandle	       (Yap_WLocal[worker_id].thread_handle)
#endif

}

static void RestoreGlobal(void) {

#if THREADS






#endif

#if defined(YAPOR) || defined(THREADS)

  REINIT_LOCK(Yap_global->bgl);
#endif




















#if HAVE_LIBREADLINE


#endif

#ifdef THREADS
  REINIT_LOCK(Yap_global->thread_handles_lock);
#endif 

#if defined(YAPOR) || defined(TABLING)


#endif







}
