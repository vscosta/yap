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
#include "Heap.h"
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

static int
allocate_new_tid(void)
{
  int new_worker_id = 0;
  LOCK(ThreadHandlesLock);
  while(new_worker_id < MAX_WORKERS &&
	ThreadHandle[new_worker_id].in_use == TRUE)
    new_worker_id++;
  ThreadHandle[new_worker_id].in_use = TRUE;
  UNLOCK(ThreadHandlesLock);
  if (new_worker_id == MAX_WORKERS) 
    return -1;
  return new_worker_id;  
}

static void
store_specs(int new_worker_id, UInt ssize, UInt tsize, Term tgoal, Term tdetach)
{
  ThreadHandle[new_worker_id].ssize = ssize;
  ThreadHandle[new_worker_id].tsize = tsize;
  ThreadHandle[new_worker_id].tgoal =
    Yap_StoreTermInDB(tgoal,4);
  ThreadHandle[new_worker_id].cmod =
    CurrentModule;
  if (IsVarTerm(tdetach)){
    ThreadHandle[new_worker_id].tdetach =  
      MkAtomTerm(AtomFalse);
  } else {
    ThreadHandle[new_worker_id].tdetach = 
      tdetach;
  }
}


static void
kill_thread_engine (int wid)
{
  Prop p0 = AbsPredProp(heap_regs->thread_handle[wid].local_preds);
  /* kill all thread local preds */
  while(p0) {
    PredEntry *ap = RepPredProp(p0);
    p0 = ap->NextOfPE;
    Yap_Abolish(ap);
    Yap_FreeCodeSpace((char *)ap);
  }
  Yap_KillStacks(wid);
  heap_regs->wl[wid].active_signals = 0L;
  free(heap_regs->wl[wid].scratchpad.ptr);
  free(ThreadHandle[wid].default_yaam_regs);
  free(ThreadHandle[wid].start_of_timesp);
  free(ThreadHandle[wid].last_timep);
  ThreadHandle[wid].in_use = FALSE;
  pthread_mutex_destroy(&(ThreadHandle[wid].tlock));
}

static void
thread_die(int wid, int always_die)
{

  LOCK(ThreadHandlesLock);
  if (!always_die) {
    /* called by thread itself */
    ThreadsTotalTime += Yap_cputime();
  }
  if (ThreadHandle[wid].tdetach == MkAtomTerm(AtomTrue) ||
      always_die)
    kill_thread_engine(wid);
  UNLOCK(ThreadHandlesLock);
}

static void
setup_engine(int myworker_id)
{
  REGSTORE *standard_regs = (REGSTORE *)malloc(sizeof(REGSTORE));
  int oldworker_id = worker_id;
  
  /* create the YAAM descriptor */
  ThreadHandle[myworker_id].default_yaam_regs = standard_regs;
  pthread_setspecific(Yap_yaamregs_key, (void *)standard_regs);
  Yap_InitExStacks(ThreadHandle[myworker_id].ssize, ThreadHandle[myworker_id].tsize);
  CurrentModule = ThreadHandle[myworker_id].cmod;
  worker_id = myworker_id;
  Yap_InitTime();
  Yap_InitYaamRegs();
  worker_id = oldworker_id;
  {
    Yap_ReleasePreAllocCodeSpace(Yap_PreAllocCodeSpace());
  }
  /* I exist */
  NOfThreadsCreated++;
}

static void
start_thread(int myworker_id)
{
  setup_engine(myworker_id);
  worker_id = myworker_id;
}

static void *
thread_run(void *widp)
{
  Term tgoal;
  Term tgs[2];
  int out;
  int myworker_id = *((int *)widp); 

  start_thread(myworker_id);
  tgs[0] = Yap_FetchTermFromDB(ThreadHandle[worker_id].tgoal);
  tgs[1] = ThreadHandle[worker_id].tdetach;
  tgoal = Yap_MkApplTerm(FunctorThreadRun, 2, tgs);
  out = Yap_RunTopGoal(tgoal);
  thread_die(worker_id, FALSE);
  return NULL;
}

static Int
p_thread_new_tid(void)
{
  return Yap_unify(MkIntegerTerm(allocate_new_tid()), ARG1);
}

static void
init_thread_engine(int new_worker_id, UInt ssize, UInt tsize, Term tgoal, Term tdetach)
{
  store_specs(new_worker_id, ssize, tsize, tgoal, tdetach);
  pthread_mutex_init(&ThreadHandle[new_worker_id].tlock, NULL);
}

static Int
p_create_thread(void)
{
  UInt ssize = IntegerOfTerm(Deref(ARG2));
  UInt tsize = IntegerOfTerm(Deref(ARG3));
  /*  UInt systemsize = IntegerOfTerm(Deref(ARG4)); */
  Term tgoal = Deref(ARG1);
  Term tdetach = Deref(ARG5);
  int new_worker_id = IntegerOfTerm(Deref(ARG6));
  
  if (new_worker_id == -1) {
    /* YAP ERROR */
    return FALSE;
  }
  init_thread_engine(new_worker_id, ssize, tsize, tgoal, tdetach);
  ThreadHandle[new_worker_id].id = new_worker_id;
  ThreadHandle[new_worker_id].ref_count = 1;
  if ((ThreadHandle[new_worker_id].ret = pthread_create(&ThreadHandle[new_worker_id].handle, NULL, thread_run, (void *)(&(ThreadHandle[new_worker_id].id)))) == 0) {
    return TRUE;
  }
  /* YAP ERROR */
  return FALSE;
}

static Int
p_thread_self(void)
{
  if (pthread_getspecific(Yap_yaamregs_key) == NULL)
    return Yap_unify(MkIntegerTerm(-1), ARG1);
  return Yap_unify(MkIntegerTerm(worker_id), ARG1);
}

int
Yap_thread_self(void)
{
  if (pthread_getspecific(Yap_yaamregs_key) == NULL)
    return -1;
  return worker_id;
}

int
Yap_thread_create_engine(thread_attr *ops)
{
  int new_id = allocate_new_tid();
  if (new_id == -1) {
    /* YAP ERROR */
    return FALSE;
  }
  init_thread_engine(new_id, ops->ssize, ops->tsize, TermNil, TermNil);
  ThreadHandle[new_id].id = new_id;
  ThreadHandle[new_id].handle = pthread_self();
  ThreadHandle[new_id].ref_count = 0;
  setup_engine(new_id);
  return TRUE;
}

int
Yap_thread_attach_engine(int wid)
{
  pthread_mutex_lock(&(ThreadHandle[wid].tlock));
  if (ThreadHandle[wid].ref_count &&
      ThreadHandle[wid].handle != pthread_self()) {
    pthread_mutex_unlock(&(ThreadHandle[wid].tlock));
    return FALSE;
  }
  ThreadHandle[wid].handle = pthread_self();
  ThreadHandle[wid].ref_count++;
  worker_id = wid;
  pthread_mutex_unlock(&(ThreadHandle[wid].tlock));
  return TRUE;
}

int
Yap_thread_detach_engine(int wid)
{
  pthread_mutex_lock(&(ThreadHandle[wid].tlock));
  if (ThreadHandle[wid].handle == worker_id)
    ThreadHandle[wid].handle = 0;
  ThreadHandle[wid].ref_count--;
  pthread_mutex_unlock(&(ThreadHandle[wid].tlock));
  return TRUE;
}

int
Yap_thread_destroy_engine(int wid)
{
  pthread_mutex_lock(&(ThreadHandle[wid].tlock));
  if (ThreadHandle[wid].ref_count == 0) {
    pthread_mutex_unlock(&(ThreadHandle[wid].tlock));
    kill_thread_engine(wid);
    return TRUE;
  } else {
    pthread_mutex_unlock(&(ThreadHandle[wid].tlock));
    return FALSE;
  }
}


static Int
p_thread_join(void)
{
  Int tid = IntegerOfTerm(Deref(ARG1));

  LOCK(ThreadHandlesLock);
  if (!ThreadHandle[tid].in_use) {
    UNLOCK(ThreadHandlesLock);
    return FALSE;
  }
  if (!ThreadHandle[tid].tdetach == MkAtomTerm(AtomTrue)) {
    UNLOCK(ThreadHandlesLock);
    return FALSE;
  }
  UNLOCK(ThreadHandlesLock);
  if (pthread_join(ThreadHandle[tid].handle, NULL) < 0) {
    /* ERROR */
    return FALSE;
  }
  return TRUE;
}

static Int
p_thread_destroy(void)
{
  Int tid = IntegerOfTerm(Deref(ARG1));

  thread_die(tid, TRUE);
  return TRUE;
}

static Int
p_thread_detach(void)
{
  if (pthread_detach(ThreadHandle[IntegerOfTerm(Deref(ARG1))].handle) < 0) {
    /* ERROR */
    return FALSE;
  }
  return TRUE;
}

static Int
p_thread_exit(void)
{
  thread_die(worker_id, FALSE);
  pthread_exit(NULL);
  return TRUE;
}

static Int
p_thread_set_concurrency(void)
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
p_valid_thread(void)
{
  Int i = IntegerOfTerm(Deref(ARG1)); 
  return ThreadHandle[i].in_use;
}

/* Mutex Support */

typedef struct swi_mutex {
  UInt owners;
  Int tid_own;
  pthread_mutex_t m;
} SWIMutex;

static Int
p_new_mutex(void)
{
  SWIMutex* mutp;
  pthread_mutexattr_t mat;

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
p_destroy_mutex(void)
{
  SWIMutex *mut = (SWIMutex*)IntegerOfTerm(Deref(ARG1));

  if (pthread_mutex_destroy(&mut->m) < 0)
    return FALSE;
  Yap_FreeCodeSpace((void *)mut);
  return TRUE;
}

static Int
p_lock_mutex(void)
{
  SWIMutex *mut = (SWIMutex*)IntegerOfTerm(Deref(ARG1));

  if (pthread_mutex_lock(&mut->m) < 0)
    return FALSE;
  mut->owners++;
  mut->tid_own = worker_id;
  return TRUE;
}

static Int
p_trylock_mutex(void)
{
  SWIMutex *mut = (SWIMutex*)IntegerOfTerm(Deref(ARG1));

  if (pthread_mutex_trylock(&mut->m) == EBUSY)
    return FALSE;
  mut->owners++;
  mut->tid_own = worker_id;
  return TRUE;
}

static Int
p_unlock_mutex(void)
{
  SWIMutex *mut = (SWIMutex*)IntegerOfTerm(Deref(ARG1));

  if (pthread_mutex_unlock(&mut->m) < 0)
    return FALSE;
  mut->owners--;
  return TRUE;
}

static Int
p_info_mutex(void)
{
  SWIMutex *mut = (SWIMutex*)IntegerOfTerm(Deref(ARG1));

  return Yap_unify(ARG2, MkIntegerTerm(mut->owners)) &&
    Yap_unify(ARG2, MkIntegerTerm(mut->tid_own));
  return TRUE;
}

static Int
p_cond_create(void)
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
p_cond_destroy(void)
{
  pthread_cond_t *condp = (pthread_cond_t *)IntegerOfTerm(Deref(ARG1));

  if (pthread_cond_destroy(condp) < 0)
    return FALSE;
  Yap_FreeCodeSpace((void *)condp);
  return TRUE;
}

static Int
p_cond_signal(void)
{
  pthread_cond_t *condp = (pthread_cond_t *)IntegerOfTerm(Deref(ARG1));

  if (pthread_cond_signal(condp) < 0)
    return FALSE;
  return TRUE;
}

static Int
p_cond_broadcast(void)
{
  pthread_cond_t *condp = (pthread_cond_t *)IntegerOfTerm(Deref(ARG1));

  if (pthread_cond_broadcast(condp) < 0)
    return FALSE;
  return TRUE;
}

static Int
p_cond_wait(void)
{
  pthread_cond_t *condp = (pthread_cond_t *)IntegerOfTerm(Deref(ARG1));
  SWIMutex *mut = (SWIMutex*)IntegerOfTerm(Deref(ARG2));

  pthread_cond_wait(condp, &mut->m);
  return TRUE;
}


static Int 
p_thread_signal(void)
{				/* '$thread_signal'(+P)	 */
  Int wid = IntegerOfTerm(Deref(ARG1));
  /* make sure the lock is available */
  pthread_mutex_lock(&(ThreadHandle[wid].tlock));
  if (!ThreadHandle[wid].in_use) {
    pthread_mutex_unlock(&(ThreadHandle[wid].tlock));
    return TRUE;
  }
  LOCK(heap_regs->wl[wid].signal_lock);
  ThreadHandle[wid].current_yaam_regs->CreepFlag_ = Unsigned(LCL0);
  heap_regs->wl[wid].active_signals |= YAP_ITI_SIGNAL;
  UNLOCK(heap_regs->wl[wid].signal_lock);
  pthread_mutex_unlock(&(ThreadHandle[wid].tlock));
  return TRUE;
}

static Int 
p_no_threads(void)
{				/* '$thread_signal'(+P)	 */
  return FALSE;
}

static Int 
p_nof_threads(void)
{				/* '$nof_threads'(+P)	 */
  int i = 0, wid;
  LOCK(ThreadHandlesLock);
  for (wid = 0; wid < MAX_WORKERS; wid++) {
    if (ThreadHandle[wid].in_use)
      i++;
  }
  UNLOCK(ThreadHandlesLock);
  return Yap_unify(ARG1,MkIntegerTerm(i));
}

static Int 
p_nof_threads_created(void)
{				/* '$nof_threads'(+P)	 */
  return Yap_unify(ARG1,MkIntTerm(NOfThreadsCreated));
}

static Int 
p_thread_runtime(void)
{				/* '$thread_runtime'(+P)	 */
  return Yap_unify(ARG1,MkIntegerTerm(ThreadsTotalTime));
}

void Yap_InitThreadPreds(void)
{
  Yap_InitCPred("$no_threads", 0, p_no_threads, HiddenPredFlag);
  Yap_InitCPred("$thread_new_tid", 1, p_thread_new_tid, HiddenPredFlag);
  Yap_InitCPred("$create_thread", 6, p_create_thread, HiddenPredFlag);
  Yap_InitCPred("$thread_self", 1, p_thread_self, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$thread_join", 1, p_thread_join, HiddenPredFlag);
  Yap_InitCPred("$thread_destroy", 1, p_thread_destroy, HiddenPredFlag);
  Yap_InitCPred("$detach_thread", 1, p_thread_detach, HiddenPredFlag);
  Yap_InitCPred("$thread_exit", 0, p_thread_exit, HiddenPredFlag);
  Yap_InitCPred("thread_setconcurrency", 2, p_thread_set_concurrency, 0);
  Yap_InitCPred("$valid_thread", 1, p_valid_thread, HiddenPredFlag);
  Yap_InitCPred("$new_mutex", 1, p_new_mutex, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$destroy_mutex", 1, p_destroy_mutex, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$lock_mutex", 1, p_lock_mutex, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$trylock_mutex", 1, p_trylock_mutex, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$unlock_mutex", 1, p_unlock_mutex, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$info_mutex", 2, p_info_mutex, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$cond_create", 1, p_cond_create, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$cond_destroy", 1, p_cond_destroy, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$cond_signal", 1, p_cond_signal, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$cond_broadcast", 1, p_cond_broadcast, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$cond_wait", 2, p_cond_wait, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$signal_thread", 1, p_thread_signal, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$nof_threads", 1, p_nof_threads, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$nof_threads_created", 1, p_nof_threads_created, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$thread_runtime", 1, p_thread_runtime, SafePredFlag|HiddenPredFlag);
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
p_nof_threads_created(void)
{				/* '$nof_threads'(+P)	 */
  return Yap_unify(ARG1,MkIntTerm(1));
}

static Int 
p_thread_runtime(void)
{				/* '$thread_runtime'(+P)	 */
  return Yap_unify(ARG1,MkIntTerm(0));
}

void Yap_InitThreadPreds(void)
{
  Yap_InitCPred("$no_threads", 0, p_no_threads, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$nof_threads", 1, p_nof_threads, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$nof_threads_created", 1, p_nof_threads_created, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$thread_runtime", 1, p_thread_runtime, SafePredFlag|HiddenPredFlag);
}


#endif /* THREADS */


