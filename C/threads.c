/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		stdpreds.c						 *
* Last rev:								 *
* mods:									 *
* comments:	threads							 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "eval.h"
#include "yapio.h"
#include <stdio.h>
#if HAVE_STRING_H
#include <string.h>
#endif

#if THREADS

#include "threads.h"

/*
 * This file includes the definition of threads in Yap. Threads
 * are supposed to be compatible with the SWI-Prolog thread package.
 *
 */

#if DEBUGX
static void DEBUG_TLOCK_ACCESS( int pos, int wid) { 
  fprintf(stderr,"wid=%p %p\n", wid, pos);
}
#else
#define DEBUG_TLOCK_ACCESS(WID, POS)
#endif

static int
allocate_new_tid(void)
{
  int new_worker_id = 0;
  LOCK(GLOBAL_ThreadHandlesLock);
  while(new_worker_id < MAX_THREADS &&
	Yap_local[new_worker_id] &&
	(REMOTE_ThreadHandle(new_worker_id).in_use == TRUE ||
	 REMOTE_ThreadHandle(new_worker_id).zombie == TRUE) )
    new_worker_id++;
  if (new_worker_id >= MAX_THREADS) {
    new_worker_id = -1;
  } else if (!Yap_local[new_worker_id]) {
    DEBUG_TLOCK_ACCESS(new_worker_id, 0);
    if (!Yap_InitThread(new_worker_id)) {
      return -1;
    }
    pthread_mutex_lock(&(REMOTE_ThreadHandle(new_worker_id).tlock));
    REMOTE_ThreadHandle(new_worker_id).in_use = TRUE;
  } else if (new_worker_id < MAX_THREADS) {
    DEBUG_TLOCK_ACCESS(new_worker_id, 0);
    pthread_mutex_lock(&(REMOTE_ThreadHandle(new_worker_id).tlock));
    REMOTE_ThreadHandle(new_worker_id).in_use = TRUE;
  } else {
    new_worker_id = -1;
  }
  UNLOCK(GLOBAL_ThreadHandlesLock);
  return new_worker_id;  
}

static int
store_specs(int new_worker_id, UInt ssize, UInt tsize, UInt sysize, Term *tpgoal, Term *tpdetach, Term *tpexit)
{
  CACHE_REGS
  UInt pm;	/* memory to be requested         */
  Term tmod;
  Term tdetach, tgoal;

  if (tsize < MinTrailSpace)
    tsize = MinTrailSpace;
  if (ssize < MinStackSpace)
    ssize = MinStackSpace;
  REMOTE_ThreadHandle(new_worker_id).ssize = ssize;
  REMOTE_ThreadHandle(new_worker_id).tsize = tsize;
  REMOTE_ThreadHandle(new_worker_id).sysize = sysize;
  REMOTE_c_input_stream(new_worker_id) = LOCAL_c_input_stream;
  REMOTE_c_output_stream(new_worker_id) = LOCAL_c_output_stream;
  REMOTE_c_error_stream(new_worker_id) = LOCAL_c_error_stream;
  pm = (ssize + tsize)*1024;
  if (!(REMOTE_ThreadHandle(new_worker_id).stack_address = malloc(pm))) {
    return FALSE;
  }
  REMOTE_ThreadHandle(new_worker_id).tgoal =
    Yap_StoreTermInDB(Deref(*tpgoal),7);
  REMOTE_ThreadHandle(new_worker_id).cmod =
    CurrentModule;
  tdetach = Deref(*tpdetach);
  if (IsVarTerm(tdetach)){
    REMOTE_ThreadHandle(new_worker_id).tdetach =  
      MkAtomTerm(AtomFalse);
  } else {
    REMOTE_ThreadHandle(new_worker_id).tdetach = 
      tdetach;
  }
  tgoal = Yap_StripModule(Deref(*tpexit), &tmod);
  REMOTE_ThreadHandle(new_worker_id).texit_mod = tmod;
  REMOTE_ThreadHandle(new_worker_id).texit =
    Yap_StoreTermInDB(tgoal,7);
  REMOTE_ThreadHandle(new_worker_id).local_preds =
    NULL;
  REMOTE_ThreadHandle(new_worker_id).start_of_timesp =
    NULL;
  REMOTE_ThreadHandle(new_worker_id).last_timep =
    NULL;
  REMOTE_ScratchPad(new_worker_id).ptr =
    NULL;
  return TRUE;
}


static void
kill_thread_engine (int wid, int always_die)
{
  Prop p0 = AbsPredProp(REMOTE_ThreadHandle(wid).local_preds);
  GlobalEntry *gl = REMOTE_GlobalVariables(wid);

  REMOTE_ThreadHandle(wid).local_preds = NIL;
  REMOTE_GlobalVariables(wid) = NULL;
  /* kill all thread local preds */
  while(p0) {
    PredEntry *ap = RepPredProp(p0);
    p0 = ap->NextOfPE;
    Yap_Abolish(ap);
    Yap_FreeCodeSpace((char *)ap);
  }
  while (gl) {
    gl->global = TermFoundVar;
    gl = gl->NextGE;
  }
  Yap_KillStacks(wid);
  REMOTE_ActiveSignals(wid) = 0L;
  if (REMOTE_ScratchPad(wid).ptr)
    free(REMOTE_ScratchPad(wid).ptr);
  REMOTE_ThreadHandle(wid).current_yaam_regs = NULL;
  if (REMOTE_ThreadHandle(wid).start_of_timesp)
    free(REMOTE_ThreadHandle(wid).start_of_timesp);
  if (REMOTE_ThreadHandle(wid).last_timep)
    free(REMOTE_ThreadHandle(wid).last_timep);
  if (REMOTE_ThreadHandle(wid).texit) {
    Yap_FreeCodeSpace((ADDR)REMOTE_ThreadHandle(wid).texit);
  }
  /* FreeCodeSpace requires LOCAL requires yaam_regs */
  free(REMOTE_ThreadHandle(wid).default_yaam_regs);
  REMOTE_ThreadHandle(wid).default_yaam_regs = NULL;
  LOCK(GLOBAL_ThreadHandlesLock);
  if (REMOTE_ThreadHandle(wid).tdetach == MkAtomTerm(AtomTrue) ||
      always_die) {
    REMOTE_ThreadHandle(wid).zombie = FALSE;
    REMOTE_ThreadHandle(wid).in_use = FALSE;
    DEBUG_TLOCK_ACCESS(1, wid);
    pthread_mutex_unlock(&(REMOTE_ThreadHandle(wid).tlock));
  }
  UNLOCK(GLOBAL_ThreadHandlesLock);
}

static void
thread_die(int wid, int always_die)
{
  if (!always_die) {
    /* called by thread itself */
    GLOBAL_ThreadsTotalTime += Yap_cputime();
  }
  kill_thread_engine(wid, always_die);
}

static int
setup_engine(int myworker_id, int init_thread)
{
  CACHE_REGS
  REGSTORE *standard_regs;
  
  standard_regs = (REGSTORE *)calloc(1,sizeof(REGSTORE));
  if (!standard_regs)
    return FALSE;
  regcache = standard_regs;
  /* create the YAAM descriptor */
  REMOTE_ThreadHandle(myworker_id).default_yaam_regs = standard_regs;
  if (init_thread) {
    pthread_setspecific(Yap_yaamregs_key, (void *)REMOTE_ThreadHandle(myworker_id).default_yaam_regs);
  }
  worker_id = myworker_id;
  LOCAL = REMOTE(worker_id);
  Yap_InitExStacks(REMOTE_ThreadHandle(myworker_id).tsize, REMOTE_ThreadHandle(myworker_id).ssize);
  CurrentModule = REMOTE_ThreadHandle(myworker_id).cmod;
  Yap_InitTime();
  Yap_InitYaamRegs();
  Yap_ReleasePreAllocCodeSpace(Yap_PreAllocCodeSpace());
  /* I exist */
  GLOBAL_NOfThreadsCreated++;
  DEBUG_TLOCK_ACCESS(2, myworker_id);
  pthread_mutex_unlock(&(REMOTE_ThreadHandle(myworker_id).tlock));  
#ifdef TABLING
  DepFr_cons_cp(LOCAL_top_dep_fr) = B;  /* same as in Yap_init_root_frames() */
#endif /* TABLING */
  return TRUE;
}

static void
start_thread(int myworker_id)
{
  setup_engine(myworker_id, TRUE);
}

static void *
thread_run(void *widp)
{
  CACHE_REGS
  Term tgoal, t;
  Term tgs[2];
  int myworker_id = *((int *)widp); 

  start_thread(myworker_id);
  regcache = ((REGSTORE *)pthread_getspecific(Yap_yaamregs_key));
  do {
    t = tgs[0] = Yap_PopTermFromDB(LOCAL_ThreadHandle.tgoal);
    if (t == 0) {
      if (LOCAL_Error_TYPE == OUT_OF_ATTVARS_ERROR) {
	LOCAL_Error_TYPE = YAP_NO_ERROR;
	if (!Yap_growglobal(NULL)) {
	  Yap_Error(OUT_OF_ATTVARS_ERROR, TermNil, LOCAL_ErrorMessage);
	  thread_die(worker_id, FALSE);
	  return NULL;
	}
      } else {
	LOCAL_Error_TYPE = YAP_NO_ERROR;
	if (!Yap_growstack(LOCAL_ThreadHandle.tgoal->NOfCells*CellSize)) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	  thread_die(worker_id, FALSE);
	  return NULL;
	}
      }
    }
  } while (t == 0);
  REMOTE_ThreadHandle(myworker_id).tgoal = NULL;
  tgs[1] = LOCAL_ThreadHandle.tdetach;
  tgoal = Yap_MkApplTerm(FunctorThreadRun, 2, tgs);
  Yap_RunTopGoal(tgoal);
  thread_die(worker_id, FALSE);
  return NULL;
}

static Int
p_thread_new_tid( USES_REGS1 )
{
  int new_worker = allocate_new_tid();
  if (new_worker == -1) {
    Yap_Error(RESOURCE_ERROR_MAX_THREADS, MkIntegerTerm(MAX_THREADS), "");
    return FALSE;
  }
  return Yap_unify(MkIntegerTerm(new_worker), ARG1);
}

static int
init_thread_engine(int new_worker_id, UInt ssize, UInt tsize, UInt sysize, Term *tgoal, Term *tdetach, Term *texit)
{
  return store_specs(new_worker_id, ssize, tsize, sysize, tgoal, tdetach, texit);
}

static Int
p_create_thread( USES_REGS1 )
{
  UInt ssize;
  UInt tsize;
  UInt sysize;
  Term x2 = Deref(ARG2);
  Term x3 = Deref(ARG3);
  Term x4 = Deref(ARG4);
  int new_worker_id = IntegerOfTerm(Deref(ARG7));
  
  //  fprintf(stderr," %d --> %d\n", worker_id, new_worker_id); 
  if (IsBigIntTerm(x2))
    return FALSE;
  if (IsBigIntTerm(x3))
    return FALSE;
  ssize = IntegerOfTerm(x2);
  tsize = IntegerOfTerm(x3);
  sysize = IntegerOfTerm(x4);
  /*  UInt systemsize = IntegerOfTerm(Deref(ARG4)); */
  if (new_worker_id == -1) {
    /* YAP ERROR */
    return FALSE;
  }
  /* make sure we can proceed */
  if (!init_thread_engine(new_worker_id, ssize, tsize, sysize, &ARG1, &ARG5, &ARG6))
    return FALSE;
  REMOTE_ThreadHandle(new_worker_id).pthread_handle = 0L;
  REMOTE_ThreadHandle(new_worker_id).id = new_worker_id;
  REMOTE_ThreadHandle(new_worker_id).ref_count = 1;
  if ((REMOTE_ThreadHandle(new_worker_id).ret = pthread_create(&REMOTE_ThreadHandle(new_worker_id).pthread_handle, NULL, thread_run, (void *)(&(REMOTE_ThreadHandle(new_worker_id).id)))) == 0) {
    /* wait until the client is initialised */
    return TRUE;
  }
  return FALSE;
}

static Int
p_thread_sleep( USES_REGS1 )
{
  UInt time = IntegerOfTerm(Deref(ARG1));
#if HAVE_NANOSLEEP
  UInt ntime = IntegerOfTerm(Deref(ARG2));
  struct timespec req, oreq ;
  req.tv_sec = time;
  req.tv_nsec = ntime;
  if (nanosleep(&req, &oreq)) {
#if HAVE_STRERROR
    Yap_Error(OPERATING_SYSTEM_ERROR, ARG1, "%s in thread_sleep/1", strerror(errno));
#else
    Yap_Error(OPERATING_SYSTEM_ERROR, ARG1, "error %d in thread_sleep/1", errno);
#endif
    return FALSE;
  }
  return Yap_unify(ARG3,MkIntegerTerm(oreq.tv_sec)) &&
    Yap_unify(ARG4,MkIntegerTerm(oreq.tv_nsec));
#elif HAVE_SLEEP
  UInt rtime;
  if ((rtime = sleep(time)) < 0) {
#if HAVE_STRERROR
    Yap_Error(OPERATING_SYSTEM_ERROR, ARG1, "%s in thread_sleep/1", strerror(errno));
#else
    Yap_Error(OPERATING_SYSTEM_ERROR, ARG1, "error %d in thread_sleep/1", errno);
#endif
  }
  return Yap_unify(ARG3,MkIntegerTerm(rtime)) &&
    Yap_unify(ARG4,MkIntTerm(0L));
#else 
  Yap_Error(OPERATING_SYSTEM_ERROR, ARG1, "no support for thread_sleep/1 in this YAP configuration");
#endif
}

static Int
p_thread_self( USES_REGS1 )
{
  if (pthread_getspecific(Yap_yaamregs_key) == NULL)
    return Yap_unify(MkIntegerTerm(-1), ARG1);
  return Yap_unify(MkIntegerTerm(worker_id), ARG1);
}


static Int
p_thread_zombie_self( USES_REGS1 )
{
  /* make sure the lock is available */
  if (pthread_getspecific(Yap_yaamregs_key) == NULL)
    return Yap_unify(MkIntegerTerm(-1), ARG1);
  DEBUG_TLOCK_ACCESS(4, worker_id);
  pthread_mutex_lock(&(LOCAL_ThreadHandle.tlock));
  if (LOCAL_ActiveSignals &= YAP_ITI_SIGNAL) {
    DEBUG_TLOCK_ACCESS(5, worker_id);
    pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
    return FALSE;
  }
  //  fprintf(stderr," -- %d\n", worker_id); 
  LOCAL_ThreadHandle.in_use = FALSE;
  LOCAL_ThreadHandle.zombie = TRUE;
  return Yap_unify(MkIntegerTerm(worker_id), ARG1);
}

static Int
p_thread_status_lock( USES_REGS1 )
{
  /* make sure the lock is available */
  if (pthread_getspecific(Yap_yaamregs_key) == NULL)
    return FALSE;
  pthread_mutex_lock(&(LOCAL_ThreadHandle.tlock_status));
  return Yap_unify(MkIntegerTerm(worker_id), ARG1);
}

static Int
p_thread_status_unlock( USES_REGS1 )
{
  /* make sure the lock is available */
  if (pthread_getspecific(Yap_yaamregs_key) == NULL)
    return FALSE;
  pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock_status));
  return Yap_unify(MkIntegerTerm(worker_id), ARG1);
}

Int
Yap_thread_self(void)
{
  CACHE_REGS
  if (pthread_getspecific(Yap_yaamregs_key) == NULL)
    return -1;
  return worker_id;
}

CELL
Yap_thread_create_engine(thread_attr *ops)
{
  thread_attr opsv;
  int new_id = allocate_new_tid();
  Term t = TermNil;

  /* 
     ok, this creates a problem, because we are initializing an engine from some "empty" thread. 
     We need first to foool the thread into believing it is the main thread
  */
  if (new_id == -1) {
    /* YAP ERROR */
    return -1;
  }
  if (ops == NULL) {
    ops = &opsv;
    ops->tsize = DefHeapSpace;
    ops->ssize = DefStackSpace;
    ops->sysize = 0;
    ops->egoal = t;
  }
  if (pthread_self() != GLOBAL_master_thread) {
    /* we are worker_id 0 for now, lock master thread so that no one messes with us */ 
    pthread_setspecific(Yap_yaamregs_key, (const void *)&Yap_standard_regs);
    pthread_mutex_lock(&(REMOTE_ThreadHandle(0).tlock));
  }
  if (!init_thread_engine(new_id, ops->ssize, ops->tsize, ops->sysize, &t, &t, &(ops->egoal)))
    return -1;
  REMOTE_ThreadHandle(new_id).pthread_handle = 0L;
  REMOTE_ThreadHandle(new_id).id = new_id;
  REMOTE_ThreadHandle(new_id).ref_count = 0;
  if (!setup_engine(new_id, FALSE))
    return -1;
  if (pthread_self() != GLOBAL_master_thread) {
    pthread_setspecific(Yap_yaamregs_key, NULL);
    pthread_mutex_unlock(&(REMOTE_ThreadHandle(0).tlock));
  }
  return new_id;
}

Int
Yap_thread_attach_engine(int wid)
{
  CACHE_REGS
  /* 
     already locked
     pthread_mutex_lock(&(REMOTE_ThreadHandle(wid).tlock));
  */
  if (REMOTE_ThreadHandle(wid).ref_count ) {
    DEBUG_TLOCK_ACCESS(8, wid);
    REMOTE_ThreadHandle(wid).ref_count++;
    REMOTE_ThreadHandle(wid).pthread_handle = pthread_self();
    pthread_mutex_unlock(&(REMOTE_ThreadHandle(wid).tlock));
    return TRUE;
  }
  REMOTE_ThreadHandle(wid).pthread_handle = pthread_self();
  REMOTE_ThreadHandle(wid).ref_count++;
  pthread_setspecific(Yap_yaamregs_key, (const void *)REMOTE_ThreadHandle(wid).default_yaam_regs);
  worker_id = wid;  /* ricroc: for what I understand, this shouldn't be necessary */
  DEBUG_TLOCK_ACCESS(9, wid);
  pthread_mutex_unlock(&(REMOTE_ThreadHandle(wid).tlock));
  return TRUE;
}

Int
Yap_thread_detach_engine(int wid)
{
  DEBUG_TLOCK_ACCESS(10, wid);
  pthread_mutex_lock(&(REMOTE_ThreadHandle(wid).tlock));
  REMOTE_ThreadHandle(wid).pthread_handle = 0;
  REMOTE_ThreadHandle(wid).ref_count--;
  pthread_setspecific(Yap_yaamregs_key, NULL);
  DEBUG_TLOCK_ACCESS(11, wid);
  pthread_mutex_unlock(&(REMOTE_ThreadHandle(wid).tlock));
  return TRUE;
}

Int
Yap_thread_destroy_engine(int wid)
{
  DEBUG_TLOCK_ACCESS(10, wid);
  pthread_mutex_lock(&(REMOTE_ThreadHandle(wid).tlock));
  if (REMOTE_ThreadHandle(wid).ref_count == 0) {
    kill_thread_engine(wid, TRUE);
    return TRUE;
  } else {
    DEBUG_TLOCK_ACCESS(12, wid);
    pthread_mutex_unlock(&(REMOTE_ThreadHandle(wid).tlock));
    return FALSE;
  }
}


static Int
p_thread_join( USES_REGS1 )
{
  Int tid = IntegerOfTerm(Deref(ARG1));

  LOCK(GLOBAL_ThreadHandlesLock);
  if (!REMOTE_ThreadHandle(tid).in_use &&
      !REMOTE_ThreadHandle(tid).zombie) {
    UNLOCK(GLOBAL_ThreadHandlesLock);
    return FALSE;
  }
  if (!REMOTE_ThreadHandle(tid).tdetach == MkAtomTerm(AtomTrue)) {
    UNLOCK(GLOBAL_ThreadHandlesLock);
    return FALSE;
  }
  UNLOCK(GLOBAL_ThreadHandlesLock);
  /* make sure this lock is accessible */
  if (pthread_join(REMOTE_ThreadHandle(tid).pthread_handle, NULL) < 0) {
    /* ERROR */
    return FALSE;
  }
  /* notice mutex is already locked */
  return TRUE;
}

static Int
p_thread_destroy( USES_REGS1 )
{
  Int tid = IntegerOfTerm(Deref(ARG1));

  LOCK(GLOBAL_ThreadHandlesLock);
  REMOTE_ThreadHandle(tid).zombie = FALSE;
  REMOTE_ThreadHandle(tid).in_use = FALSE;
  DEBUG_TLOCK_ACCESS(32, tid);
  pthread_mutex_unlock(&(REMOTE_ThreadHandle(tid).tlock));
  UNLOCK(GLOBAL_ThreadHandlesLock);
  return TRUE;
}

static Int
p_thread_detach( USES_REGS1 )
{
  Int tid = IntegerOfTerm(Deref(ARG1));
  pthread_mutex_lock(&(REMOTE_ThreadHandle(tid).tlock));
  DEBUG_TLOCK_ACCESS(14, tid);
  if (pthread_detach(REMOTE_ThreadHandle(tid).pthread_handle) < 0) {
    /* ERROR */
    DEBUG_TLOCK_ACCESS(15, tid);
    pthread_mutex_unlock(&(REMOTE_ThreadHandle(tid).tlock));
    return FALSE;
  }
  REMOTE_ThreadHandle(tid).tdetach = 
    MkAtomTerm(AtomTrue);
  DEBUG_TLOCK_ACCESS(30, tid);
  pthread_mutex_unlock(&(REMOTE_ThreadHandle(tid).tlock));
  return TRUE;
}

static Int
p_thread_detached( USES_REGS1 )
{
  if (LOCAL_ThreadHandle.tdetach)
    return Yap_unify(ARG1,LOCAL_ThreadHandle.tdetach);
  else
    return FALSE;
}

static Int
p_thread_detached2( USES_REGS1 )
{
  Int tid = IntegerOfTerm(Deref(ARG1));
  if (REMOTE_ThreadHandle(tid).tdetach)
    return Yap_unify(ARG2,REMOTE_ThreadHandle(tid).tdetach);
  else
    return FALSE;
}

static Int
p_thread_exit( USES_REGS1 )
{
  thread_die(worker_id, FALSE); 
  pthread_exit(NULL);
  /* done, just make gcc happy */
  return TRUE;
}

static Int
p_thread_set_concurrency( USES_REGS1 )
{
  Term tnew = Deref(ARG2);
  int newc, cur;

  if (IsVarTerm(tnew)) {
    newc = 0;
  } else if (IsIntegerTerm(tnew)) {
    newc = IntegerOfTerm(tnew);
  } else {
    Yap_Error(TYPE_ERROR_INTEGER,tnew,"thread_set_concurrency/2");
    return(FALSE);
  }
  cur = MkIntegerTerm(pthread_getconcurrency());
  if (pthread_setconcurrency(newc) != 0) {
    return FALSE;
  }
  return Yap_unify(ARG1, MkIntegerTerm(cur));
}

static Int
p_thread_yield( USES_REGS1 )
{
  if (sched_yield() != 0) {
    return FALSE;
  }
  return TRUE;
}

static Int
p_valid_thread( USES_REGS1 )
{
  Int i = IntegerOfTerm(Deref(ARG1)); 
  return REMOTE_ThreadHandle(i).in_use || REMOTE_ThreadHandle(i).zombie;
}

/* Mutex Support */

typedef struct swi_mutex {
  UInt owners;
  Int tid_own;
  pthread_mutex_t m;
} SWIMutex;

static Int
p_new_mutex( USES_REGS1 )
{
  SWIMutex* mutp;
  pthread_mutexattr_t mat;
#ifdef HAVE_PTHREAD_MUTEXATTR_SETKIND_NP
  extern int pthread_mutexattr_setkind_np(pthread_mutexattr_t *attr, int kind);
#endif

  mutp = (SWIMutex *)Yap_AllocCodeSpace(sizeof(SWIMutex));
  if (mutp == NULL) {
    return FALSE;
  }
  pthread_mutexattr_init(&mat);
#ifdef HAVE_PTHREAD_MUTEXATTR_SETKIND_NP
  pthread_mutexattr_setkind_np(&mat, PTHREAD_MUTEX_RECURSIVE_NP);
#else
#ifdef HAVE_PTHREAD_MUTEXATTR_SETTYPE
  pthread_mutexattr_settype(&mat, PTHREAD_MUTEX_RECURSIVE);
#endif
#endif
  pthread_mutex_init(&mutp->m, &mat);
  mutp->owners = 0;
  mutp->tid_own = 0;
  return Yap_unify(ARG1, MkIntegerTerm((Int)mutp));
}

static Int
p_destroy_mutex( USES_REGS1 )
{
  SWIMutex *mut = (SWIMutex*)IntegerOfTerm(Deref(ARG1));

  if (pthread_mutex_destroy(&mut->m) < 0)
    return FALSE;
  Yap_FreeCodeSpace((void *)mut);
  return TRUE;
}

static Int
p_lock_mutex( USES_REGS1 )
{
  SWIMutex *mut = (SWIMutex*)IntegerOfTerm(Deref(ARG1));

  if (pthread_mutex_lock(&mut->m) < 0)
    return FALSE;
  mut->owners++;
  mut->tid_own = worker_id;
  return TRUE;
}

static Int
p_trylock_mutex( USES_REGS1 )
{
  SWIMutex *mut = (SWIMutex*)IntegerOfTerm(Deref(ARG1));

  if (pthread_mutex_trylock(&mut->m) == EBUSY)
    return FALSE;
  mut->owners++;
  mut->tid_own = worker_id;
  return TRUE;
}

static Int
p_unlock_mutex( USES_REGS1 )
{
  SWIMutex *mut = (SWIMutex*)IntegerOfTerm(Deref(ARG1));

  if (pthread_mutex_unlock(&mut->m) < 0)
    return FALSE;
  mut->owners--;
  return TRUE;
}

static Int
p_mutex_info( USES_REGS1 )
{
  SWIMutex *mut = (SWIMutex*)IntegerOfTerm(Deref(ARG1));

  return Yap_unify(ARG2, MkIntegerTerm(mut->owners)) &&
    Yap_unify(ARG3, MkIntegerTerm(mut->tid_own));
  return TRUE;
}

static Int
p_cond_create( USES_REGS1 )
{
  pthread_cond_t* condp;

  condp = (pthread_cond_t *)Yap_AllocCodeSpace(sizeof(pthread_cond_t));
  if (condp == NULL) {
    return FALSE;
  }
  pthread_cond_init(condp, NULL);
  return Yap_unify(ARG1, MkIntegerTerm((Int)condp));
}

static Int
p_cond_destroy( USES_REGS1 )
{
  pthread_cond_t *condp = (pthread_cond_t *)IntegerOfTerm(Deref(ARG1));

  if (pthread_cond_destroy(condp) < 0)
    return FALSE;
  Yap_FreeCodeSpace((void *)condp);
  return TRUE;
}

static Int
p_cond_signal( USES_REGS1 )
{
  pthread_cond_t *condp = (pthread_cond_t *)IntegerOfTerm(Deref(ARG1));

  if (pthread_cond_signal(condp) < 0)
    return FALSE;
  return TRUE;
}

static Int
p_cond_broadcast( USES_REGS1 )
{
  pthread_cond_t *condp = (pthread_cond_t *)IntegerOfTerm(Deref(ARG1));

  if (pthread_cond_broadcast(condp) < 0)
    return FALSE;
  return TRUE;
}

static Int
p_cond_wait( USES_REGS1 )
{
  pthread_cond_t *condp = (pthread_cond_t *)IntegerOfTerm(Deref(ARG1));
  SWIMutex *mut = (SWIMutex*)IntegerOfTerm(Deref(ARG2));

  pthread_cond_wait(condp, &mut->m);
  return TRUE;
}

static Int 
p_thread_stacks( USES_REGS1 )
{				/* '$thread_signal'(+P)	 */
  Int tid = IntegerOfTerm(Deref(ARG1));
  Int status= TRUE;

  LOCK(GLOBAL_ThreadHandlesLock);
  if (!REMOTE_ThreadHandle(tid).in_use &&
      !REMOTE_ThreadHandle(tid).zombie) {
    UNLOCK(GLOBAL_ThreadHandlesLock);
    return FALSE;
  }
  status &= Yap_unify(ARG2,MkIntegerTerm(REMOTE_ThreadHandle(tid).ssize));
  status &= Yap_unify(ARG3,MkIntegerTerm(REMOTE_ThreadHandle(tid).tsize));
  status &= Yap_unify(ARG4,MkIntegerTerm(REMOTE_ThreadHandle(tid).sysize));
  UNLOCK(GLOBAL_ThreadHandlesLock);
  return status;
}

static Int 
p_thread_atexit( USES_REGS1 )
{				/* '$thread_signal'(+P)	 */
  Term t;

  if (!LOCAL_ThreadHandle.texit ||
      LOCAL_ThreadHandle.texit->Entry == MkAtomTerm(AtomTrue)) {
    return FALSE;
  }
  do {
    t = Yap_PopTermFromDB(LOCAL_ThreadHandle.texit);
    LOCAL_ThreadHandle.texit = NULL;
    if (t == 0) {
      if (LOCAL_Error_TYPE == OUT_OF_ATTVARS_ERROR) {
	LOCAL_Error_TYPE = YAP_NO_ERROR;
	if (!Yap_growglobal(NULL)) {
	  Yap_Error(OUT_OF_ATTVARS_ERROR, TermNil, LOCAL_ErrorMessage);
	  thread_die(worker_id, FALSE);
	  return FALSE;
	}
      } else {
	LOCAL_Error_TYPE = YAP_NO_ERROR;
	if (!Yap_growstack(LOCAL_ThreadHandle.tgoal->NOfCells*CellSize)) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	  thread_die(worker_id, FALSE);
	  return FALSE;
	}
      }
    }
  } while (t == 0);
  return Yap_unify(ARG1, t) && Yap_unify(ARG2, LOCAL_ThreadHandle.texit_mod);
}



static Int 
p_thread_signal( USES_REGS1 )
{				/* '$thread_signal'(+P)	 */
  Int wid = IntegerOfTerm(Deref(ARG1));
  /* make sure the lock is available */
  pthread_mutex_lock(&(REMOTE_ThreadHandle(wid).tlock));
  DEBUG_TLOCK_ACCESS(16, wid);
  if (!REMOTE_ThreadHandle(wid).in_use || 
      !REMOTE_ThreadHandle(wid).current_yaam_regs) {
    DEBUG_TLOCK_ACCESS(17, wid);
    pthread_mutex_unlock(&(REMOTE_ThreadHandle(wid).tlock));
   return TRUE;
  }
  LOCK(REMOTE_SignalLock(wid));
  REMOTE_ThreadHandle(wid).current_yaam_regs->CreepFlag_ = 
    Unsigned(REMOTE_ThreadHandle(wid).current_yaam_regs->LCL0_);
  REMOTE_ActiveSignals(wid) |= YAP_ITI_SIGNAL;
  UNLOCK(REMOTE_SignalLock(wid));
  DEBUG_TLOCK_ACCESS(18, wid);
  pthread_mutex_unlock(&(REMOTE_ThreadHandle(wid).tlock));
  return TRUE;
}

static Int 
p_no_threads( USES_REGS1 )
{				/* '$thread_signal'(+P)	 */
  return FALSE;
}

static Int 
p_nof_threads( USES_REGS1 )
{				/* '$nof_threads'(+P)	 */
  int i = 0, wid;
  LOCK(GLOBAL_ThreadHandlesLock);
  for (wid = 0; wid < MAX_THREADS; wid++) {
    if (!Yap_local[wid]) break;
    if (REMOTE_ThreadHandle(wid).in_use)
      i++;
  }
  UNLOCK(GLOBAL_ThreadHandlesLock);
  return Yap_unify(ARG1,MkIntegerTerm(i));
}

static Int 
p_max_workers( USES_REGS1 )
{				/* '$max_workers'(+P)	 */
  return Yap_unify(ARG1,MkIntegerTerm(MAX_WORKERS));
}

static Int 
p_max_threads( USES_REGS1 )
{				/* '$max_threads'(+P)	 */
  return Yap_unify(ARG1,MkIntegerTerm(MAX_THREADS));
}

static Int 
p_nof_threads_created( USES_REGS1 )
{				/* '$nof_threads'(+P)	 */
  return Yap_unify(ARG1,MkIntTerm(GLOBAL_NOfThreadsCreated));
}

static Int 
p_thread_runtime( USES_REGS1 )
{				/* '$thread_runtime'(+P)	 */
  return Yap_unify(ARG1,MkIntegerTerm(GLOBAL_ThreadsTotalTime));
}

static Int 
p_thread_self_lock( USES_REGS1 )
{				/* '$thread_unlock'	 */
  pthread_mutex_lock(&(LOCAL_ThreadHandle.tlock));
  return Yap_unify(ARG1,MkIntegerTerm(worker_id));
}

static Int 
p_thread_unlock( USES_REGS1 )
{				/* '$thread_unlock'	 */
  Int wid = IntegerOfTerm(Deref(ARG1));
  DEBUG_TLOCK_ACCESS(19, wid);
  pthread_mutex_unlock(&(REMOTE_ThreadHandle(wid).tlock));
  return TRUE;
}

void Yap_InitThreadPreds(void)
{
  Yap_InitCPred("$no_threads", 0, p_no_threads, HiddenPredFlag);
  Yap_InitCPred("$max_workers", 1, p_max_workers, HiddenPredFlag);
  Yap_InitCPred("$max_threads", 1, p_max_threads, HiddenPredFlag);
  Yap_InitCPred("$thread_new_tid", 1, p_thread_new_tid, HiddenPredFlag);
  Yap_InitCPred("$create_thread", 7, p_create_thread, HiddenPredFlag);
  Yap_InitCPred("$thread_self", 1, p_thread_self, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$thread_status_lock", 1, p_thread_status_lock, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$thread_status_unlock", 1, p_thread_status_unlock, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$thread_zombie_self", 1, p_thread_zombie_self, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$thread_join", 1, p_thread_join, HiddenPredFlag);
  Yap_InitCPred("$thread_destroy", 1, p_thread_destroy, HiddenPredFlag);
  Yap_InitCPred("thread_yield", 0, p_thread_yield, 0);
  Yap_InitCPred("$detach_thread", 1, p_thread_detach, HiddenPredFlag);
  Yap_InitCPred("$thread_detached", 1, p_thread_detached, HiddenPredFlag);
  Yap_InitCPred("$thread_detached", 2, p_thread_detached2, HiddenPredFlag);
  Yap_InitCPred("$thread_exit", 0, p_thread_exit, HiddenPredFlag);
  Yap_InitCPred("thread_setconcurrency", 2, p_thread_set_concurrency, 0);
  Yap_InitCPred("$valid_thread", 1, p_valid_thread, HiddenPredFlag);
  Yap_InitCPred("$new_mutex", 1, p_new_mutex, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$destroy_mutex", 1, p_destroy_mutex, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$lock_mutex", 1, p_lock_mutex, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$trylock_mutex", 1, p_trylock_mutex, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$unlock_mutex", 1, p_unlock_mutex, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$mutex_info", 3, p_mutex_info, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$cond_create", 1, p_cond_create, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$cond_destroy", 1, p_cond_destroy, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$cond_signal", 1, p_cond_signal, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$cond_broadcast", 1, p_cond_broadcast, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$cond_wait", 2, p_cond_wait, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$thread_stacks", 4, p_thread_stacks, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$signal_thread", 1, p_thread_signal, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$nof_threads", 1, p_nof_threads, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$nof_threads_created", 1, p_nof_threads_created, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$thread_sleep", 4, p_thread_sleep, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$thread_runtime", 1, p_thread_runtime, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$thread_self_lock", 1, p_thread_self_lock, SafePredFlag);
  Yap_InitCPred("$thread_run_at_exit", 2, p_thread_atexit, SafePredFlag);
  Yap_InitCPred("$thread_unlock", 1, p_thread_unlock, SafePredFlag);
}

#else

static Int 
p_no_threads(void)
{				/* '$thread_signal'(+P)	 */
  return TRUE;
}

static Int 
p_nof_threads(void)
{				/* '$nof_threads'(+P)	 */
  return Yap_unify(ARG1,MkIntTerm(1));
}

static Int 
p_max_threads(void)
{				/* '$nof_threads'(+P)	 */
  return Yap_unify(ARG1,MkIntTerm(1));
}

static Int 
p_nof_threads_created(void)
{				/* '$nof_threads'(+P)	 */
  return Yap_unify(ARG1,MkIntTerm(1));
}

static Int 
p_thread_runtime(void)
{				/* '$thread_runtime'(+P)	 */
  return Yap_unify(ARG1,MkIntTerm(0));
}

static Int 
p_thread_self(void)
{				/* '$thread_runtime'(+P)	 */
  return Yap_unify(ARG1,MkIntTerm(0));
}

static Int
p_thread_stacks(void)
{				/* '$thread_runtime'(+P)	 */
  return FALSE;
}

static Int 
p_thread_unlock(void)
{				/* '$thread_runtime'(+P)	 */
  return TRUE;
}

static Int 
p_max_workers(void)
{				/* '$max_workers'(+P)	 */
  return Yap_unify(ARG1,MkIntTerm(1));
}

void Yap_InitThreadPreds(void)
{
  Yap_InitCPred("$max_workers", 1, p_max_workers, HiddenPredFlag);
  Yap_InitCPred("$thread_self", 1, p_thread_self, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$no_threads", 0, p_no_threads, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$max_threads", 1, p_max_threads, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$nof_threads", 1, p_nof_threads, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$nof_threads_created", 1, p_nof_threads_created, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$thread_stacks", 4, p_thread_stacks, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$thread_runtime", 1, p_thread_runtime, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$thread_unlock", 1, p_thread_unlock, SafePredFlag);
}


#endif /* THREADS */


