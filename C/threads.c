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

#if THREADS

/*
 * This file includes the definition of threads in Yap. Threads
 * are supposed to be compatible with the SWI-Prolog thread package.
 *
 */

#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#include "eval.h"
#include "yapio.h"
#include <stdio.h>
#if HAVE_STRING_H
#include <string.h>
#endif

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
store_specs(int new_worker_id, UInt ssize, UInt tsize, Term tgoal)
{
  ThreadHandle[new_worker_id].ssize = ssize;
  ThreadHandle[new_worker_id].tsize = tsize;
  ThreadHandle[new_worker_id].tgoal =
    Yap_StoreTermInDB(tgoal,4);
}


static void
thread_die(void)
{
  Yap_KillStacks();
  LOCK(ThreadHandlesLock);
  ThreadHandle[worker_id].in_use = FALSE;
  free((void *)ThreadHandle[worker_id].default_yaam_regs);
  UNLOCK(ThreadHandlesLock);
}

static void *
thread_run(void *widp)
{
  Term tgoal;
  Term tgs[1];
  int out;
  REGSTORE *standard_regs = (REGSTORE *)malloc(sizeof(REGSTORE));
  int myworker_id = *((int *)widp); 
  
  /* create the YAAM descriptor */
  ThreadHandle[myworker_id].default_yaam_regs = standard_regs;
  pthread_setspecific(yaamregs_key, (void *)standard_regs);
  worker_id = myworker_id;
  Yap_InitExStacks(ThreadHandle[myworker_id].ssize, ThreadHandle[myworker_id].tsize);
  Yap_InitYaamRegs();
  {
    ADDR ptr = Yap_PreAllocCodeSpace();
    Yap_ReleasePreAllocCodeSpace(ptr);
  }
  tgs[0] = Yap_FetchTermFromDB(ThreadHandle[worker_id].tgoal);
  tgoal = Yap_MkApplTerm(FunctorThreadRun, 1, tgs);
  out = Yap_RunTopGoal(tgoal);
  thread_die();
  return NULL;
}

static Int
p_create_thread(void)
{
  UInt ssize = IntegerOfTerm(Deref(ARG2));
  UInt tsize = IntegerOfTerm(Deref(ARG3));
  /*  UInt systemsize = IntegerOfTerm(Deref(ARG4)); */
  Term tgoal = Deref(ARG1);
  int new_worker_id = allocate_new_tid();
  if (new_worker_id == -1) {
    /* YAP ERROR */
    return FALSE;
  }    
  ThreadHandle[new_worker_id].id = new_worker_id;
  store_specs(new_worker_id, ssize, tsize, tgoal);
  if ((ThreadHandle[new_worker_id].ret = pthread_create(&(ThreadHandle[new_worker_id].handle), NULL, thread_run, (void *)(&(ThreadHandle[new_worker_id].id)))) == 0)
    return Yap_unify(MkIntegerTerm(new_worker_id), ARG5);
  thread_die();
  /* YAP ERROR */
  return FALSE;
}

static Int
p_thread_self(void)
{
  return Yap_unify(MkIntegerTerm(worker_id), ARG1);
}

static Int
p_thread_join(void)
{
  pthread_t th = ThreadHandle[IntegerOfTerm(Deref(ARG1))].handle;
  void *retval;
  if (pthread_join(th, &retval) < 0) {
    /* ERROR */
    return FALSE;
  }
  return TRUE;
}

static Int
p_thread_detach(void)
{
  pthread_t th = ThreadHandle[IntegerOfTerm(Deref(ARG1))].handle;
  if (pthread_detach(th) < 0) {
    /* ERROR */
    return FALSE;
  }
  return TRUE;
}

static Int
p_thread_exit(void)
{
  pthread_exit(NULL);
  return TRUE;
}

static Int
p_thread_die(void)
{
  thread_die();
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
  SWIMutex *mut = (SWIMutex*)Deref(ARG1);

  if (pthread_mutex_destroy(&mut->m) < 0)
    return FALSE;
  Yap_FreeCodeSpace((void *)mut);
  return TRUE;
}

static Int
p_lock_mutex(void)
{
  SWIMutex *mut = (SWIMutex*)Deref(ARG1);

  if (pthread_mutex_lock(&mut->m) < 0)
    return FALSE;
  mut->owners++;
  mut->tid_own = worker_id;
  return TRUE;
}

static Int
p_trylock_mutex(void)
{
  SWIMutex *mut = (SWIMutex*)Deref(ARG1);

  if (pthread_mutex_trylock(&mut->m) == EBUSY)
    return FALSE;
  mut->owners++;
  mut->tid_own = worker_id;
  return TRUE;
}

static Int
p_unlock_mutex(void)
{
  SWIMutex *mut = (SWIMutex*)Deref(ARG1);

  if (pthread_mutex_unlock(&mut->m) < 0)
    return FALSE;
  mut->owners--;
  return TRUE;
}

static Int
p_info_mutex(void)
{
  SWIMutex *mut = (SWIMutex*)Deref(ARG1);

  return Yap_unify(ARG2, MkIntegerTerm(mut->owners)) &&
    Yap_unify(ARG2, MkIntegerTerm(mut->tid_own));
  return TRUE;
}

static Int
p_cond_create(void)
{
  pthread_cond_t* condp;

  condp = (SWIMutex *)Yap_AllocCodeSpace(sizeof(pthread_cond_t));
  if (condp == NULL) {
    return FALSE;
  }
  pthread_cond_init(condp, NULL);
  return Yap_unify(ARG1, MkIntegerTerm((Int)condp));
}

static Int
p_cond_destroy(void)
{
  pthread_cond_t *condp = (pthread_cond_t *)Deref(ARG1);

  if (pthread_cond_destroy(condp) < 0)
    return FALSE;
  Yap_FreeCodeSpace((void *)condp);
  return TRUE;
}

static Int
p_cond_signal(void)
{
  pthread_cond_t *condp = (pthread_cond_t *)Deref(ARG1);

  if (pthread_cond_signal(condp) < 0)
    return FALSE;
  return TRUE;
}

static Int
p_cond_broadcast(void)
{
  pthread_cond_t *condp = (pthread_cond_t *)Deref(ARG1);

  if (pthread_cond_broadcast(condp) < 0)
    return FALSE;
  return TRUE;
}

static Int
p_cond_wait(void)
{
  pthread_cond_t *condp = (pthread_cond_t *)Deref(ARG1);
  SWIMutex *mut = (SWIMutex*)Deref(ARG2);

  if (pthread_cond_wait(condp, &mut->m) < 0)
    return FALSE;
  return TRUE;
}

void Yap_InitThreadPreds(void)
{
  Yap_InitCPred("$create_thread", 5, p_create_thread, 0);
  Yap_InitCPred("$thread_self", 1, p_thread_self, SafePredFlag);
  Yap_InitCPred("$thread_join", 1, p_thread_join, 0);
  Yap_InitCPred("$detach_thread", 1, p_thread_detach, 0);
  Yap_InitCPred("$thread_exit", 0, p_thread_exit, 0);
  Yap_InitCPred("$thread_die", 0, p_thread_die, 0);
  Yap_InitCPred("thread_set_concurrency", 2, p_thread_set_concurrency, 0);
  Yap_InitCPred("$valid_thread", 1, p_valid_thread, 0);
  Yap_InitCPred("$new_mutex", 1, p_new_mutex, SafePredFlag);
  Yap_InitCPred("$destroy_mutex", 1, p_destroy_mutex, SafePredFlag);
  Yap_InitCPred("$lock_mutex", 1, p_lock_mutex, SafePredFlag);
  Yap_InitCPred("$trylock_mutex", 1, p_trylock_mutex, SafePredFlag);
  Yap_InitCPred("$unlock_mutex", 1, p_unlock_mutex, SafePredFlag);
  Yap_InitCPred("$info_mutex", 2, p_info_mutex, SafePredFlag);
  Yap_InitCPred("$cond_create", 1, p_cond_create, SafePredFlag);
  Yap_InitCPred("$cond_destroy", 1, p_cond_destroy, SafePredFlag);
  Yap_InitCPred("$cond_signal", 1, p_cond_signal, SafePredFlag);
  Yap_InitCPred("$cond_broadcast", 1, p_cond_broadcast, SafePredFlag);
  Yap_InitCPred("$cond_wait", 2, p_cond_wait, SafePredFlag);
}


#endif /* THREADS */


