
  /* This file, rlocals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/LOCALS instead */


static void RestoreWorker(int wid USES_REGS) {










































  REMOTE(wid)->global_arena = TermToGlobalOrAtomAdjust(REMOTE(wid)->global_arena);








  RestoreBallTerm(wid);







#ifdef  COROUTINING
  REMOTE(wid)->woken_goals = TermToGlobalAdjust(REMOTE(wid)->woken_goals);
  REMOTE(wid)->atts_mutable_list = TermToGlobalAdjust(REMOTE(wid)->atts_mutable_list);
#endif

  REMOTE(wid)->gc_generation = TermToGlobalAdjust(REMOTE(wid)->gc_generation);
  REMOTE(wid)->gc_phase = TermToGlobalAdjust(REMOTE(wid)->gc_phase);






#if LOW_LEVEL_TRACER

#endif

#if defined(YAPOR) || defined(THREADS)
  REINIT_LOCK(REMOTE(wid)->signal_lock);
#endif








#if defined(GC_NO_TAGS)

#endif















  REMOTE(wid)->dynamic_arrays = PtoArrayEAdjust(REMOTE(wid)->dynamic_arrays);
  REMOTE(wid)->static_arrays = PtoArraySAdjust(REMOTE(wid)->static_arrays);
  REMOTE(wid)->global_variables = PtoGlobalEAdjust(REMOTE(wid)->global_variables);










#ifdef THREADS

#define FOREIGN_ThreadHandle(wid)  			(Yap_local[(wid)]->thread_handle)
#endif /* THREADS */
#if defined(YAPOR) || defined(TABLING)

#endif /* YAPOR || TABLING */

}
