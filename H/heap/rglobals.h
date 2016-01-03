
  /* This file, rglobals.h, was generated automatically by "yap -L misc/buildlocalglobal"
     please do not update, update misc/GLOBALS instead */

















static void RestoreGlobal(void) {

















#if __ANDROID__



#endif

#if THREADS







  REINIT_LOCK(GLOBAL_ThreadHandlesLock);
#endif	
#if defined(YAPOR) || defined(THREADS)

  REINIT_LOCK(GLOBAL_BGL);
#endif
#if defined(YAPOR) || defined(TABLING)

#endif /* YAPOR || TABLING */



#if defined(THREADS)


  REINIT_LOCK(GLOBAL_mboxq_lock);


#endif /* THREADS */


#if defined(THREADS)||defined(YAPOR)
  REINIT_LOCK(GLOBAL_StreamDescLock);
#endif




#ifdef COROUTINING


#endif








#if HAVE_MMAP

#endif
#ifdef DEBUG





#endif
#if defined(COFF)  || defined(A_OUT)


#endif


#if  __simplescalar__

#endif















#if LOW_PROF



#endif /* LOW_PROF */

#if THREADS


  REINIT_LOCK(GLOBAL_MUT_ACCESS);
#endif








}
