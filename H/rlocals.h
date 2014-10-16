
  /* This file, rlocals.h, was generated automatically by "yap -L misc/buildlocalglobal"
     please do not update, update misc/LOCALS instead */


static void RestoreWorker(int wid USES_REGS) {










































  REMOTE_GlobalArena(wid) = TermToGlobalOrAtomAdjust(REMOTE_GlobalArena(wid));







  RestoreBallTerm(wid);








#ifdef  COROUTINING
  REMOTE_WokenGoals(wid) = TermToGlobalAdjust(REMOTE_WokenGoals(wid));
  REMOTE_AttsMutableList(wid) = TermToGlobalAdjust(REMOTE_AttsMutableList(wid));
#endif

  REMOTE_GcGeneration(wid) = TermToGlobalAdjust(REMOTE_GcGeneration(wid));
  REMOTE_GcPhase(wid) = TermToGlobalAdjust(REMOTE_GcPhase(wid));














#if defined(GC_NO_TAGS)

#endif



















  REMOTE_DynamicArrays(wid) = PtoArrayEAdjust(REMOTE_DynamicArrays(wid));
  REMOTE_StaticArrays(wid) = PtoArraySAdjust(REMOTE_StaticArrays(wid));
  REMOTE_GlobalVariables(wid) = PtoGlobalEAdjust(REMOTE_GlobalVariables(wid));











#ifdef THREADS

#endif /* THREADS */
#if defined(YAPOR) || defined(TABLING)

#endif /* YAPOR || TABLING */


#if LOW_LEVEL_TRACER

#endif








#ifdef THREADS

#else

#endif	





















#ifdef ANALYST


#endif /* ANALYST */



















#ifdef LOAD_DYLD

#endif

#ifdef LOW_LEVEL_TRACER

#endif





























#if __ANDROID__



#endif












}
