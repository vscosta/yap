
  /* This file, rlocals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/LOCALS instead */


static void RestoreWorker(int wid USES_REGS) {










































  FOREIGN(wid)->global_arena = TermToGlobalOrAtomAdjust(FOREIGN(wid)->global_arena);








  RestoreBallTerm(wid);







#ifdef  COROUTINING
  FOREIGN(wid)->woken_goals = TermToGlobalAdjust(FOREIGN(wid)->woken_goals);
  FOREIGN(wid)->atts_mutable_list = TermToGlobalAdjust(FOREIGN(wid)->atts_mutable_list);
#endif

  FOREIGN(wid)->gc_generation = TermToGlobalAdjust(FOREIGN(wid)->gc_generation);
  FOREIGN(wid)->gc_phase = TermToGlobalAdjust(FOREIGN(wid)->gc_phase);






#if LOW_LEVEL_TRACER

#endif


#if defined(YAPOR) || defined(THREADS)
  REINIT_LOCK(FOREIGN(wid)->signal_lock);




#if DEBUG && COROUTINING

#endif




#if GC_NO_TAGS

#endif
#if defined(TABLING) || defined(YAPOR_SBA)



#else



#endif











#endif /* defined(YAPOR) || defined(THREADS) */

  FOREIGN(wid)->dynamic_arrays = PtoArrayEAdjust(FOREIGN(wid)->dynamic_arrays);
  FOREIGN(wid)->static_arrays = PtoArraySAdjust(FOREIGN(wid)->static_arrays);
  FOREIGN(wid)->global_variables = PtoGlobalEAdjust(FOREIGN(wid)->global_variables);










#ifdef THREADS

#define FOREIGN_ThreadHandle(wid)  (Yap_WLocal[(wid)]->thread_handle)		       						
#define MY_ThreadHandle	       (Yap_WLocal[worker_id]->thread_handle)
#endif

}
