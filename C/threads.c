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

/**
@ingroup Threads 
@{
*/

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "eval.h"
#include "yapio.h"
#include "pl-shared.h"
#include <stdio.h>
#include <SWI-Prolog.h>
#if HAVE_STRING_H
#include <string.h>
#endif
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */


PL_blob_t PL_Message_Queue = {
  PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE | PL_BLOB_NOCOPY,
  "message_queue",
  0, // release
  0, // compare
  0, // write
  0 // acquire
};


#if DEBUG_LOCKS||DEBUG_PE_LOCKS

int debug_locks = FALSE, debug_pe_locks = FALSE;
static Int p_debug_locks( USES_REGS1 ) { debugf=stdout; debug_pe_locks = 1; return TRUE; }

static Int p_nodebug_locks( USES_REGS1 ) { debug_locks = 0; debug_pe_locks = 0; return TRUE; }

#endif

#if THREADS

#include "threads.h"


/*
 * This file includes the definition of threads in Yap. Threads
 * are supposed to be compatible with the SWI-Prolog thread package.
 *
 */

static void
set_system_thread_id(int wid, PL_thread_info_t *info)
{
  if (!info)
    info = (PL_thread_info_t *)malloc(sizeof(PL_thread_info_t));
  info = SWI_thread_info(wid, info);
  info->tid = pthread_self();
  info->has_tid = TRUE;
#ifdef HAVE_GETTID_SYSCALL
  info->pid = syscall(__NR_gettid);
#else
#ifdef HAVE_GETTID_MACRO
  info->pid = gettid();
#else
#ifdef __WINDOWS__
  info->w32id = GetCurrentThreadId();
#endif
#endif
#endif
}

int
Yap_ThreadID( void )
{
  int new_worker_id = 0;
  pthread_t self = pthread_self();
  while(new_worker_id < MAX_THREADS &&
	Yap_local[new_worker_id] &&
	(REMOTE_ThreadHandle(new_worker_id).in_use == TRUE ||
	 REMOTE_ThreadHandle(new_worker_id).zombie == TRUE) ) {
    if (pthread_equal(self , REMOTE_ThreadHandle(new_worker_id).pthread_handle) ) {
      return new_worker_id;
    }
    new_worker_id++;
  }
  return -1;
}

int
Yap_NOfThreads(void) {
  // GLOBAL_ThreadHandlesLock is held
  return GLOBAL_NOfThreads;
}

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
    if (!Yap_InitThread(new_worker_id)) {
      UNLOCK(GLOBAL_ThreadHandlesLock);
      return -1;
    }
    MUTEX_LOCK(&(REMOTE_ThreadHandle(new_worker_id).tlock));
    REMOTE_ThreadHandle(new_worker_id).in_use = TRUE;
  } else if (new_worker_id < MAX_THREADS) {
    // reuse existing thread
    MUTEX_LOCK(&(REMOTE_ThreadHandle(new_worker_id).tlock));
    REMOTE_ThreadHandle(new_worker_id).in_use = TRUE;
  } else {
    new_worker_id = -1;
  }
  UNLOCK(GLOBAL_ThreadHandlesLock);
  return new_worker_id;  
}


static bool
mboxCreate( Term namet, mbox_t *mboxp USES_REGS )
{
  pthread_mutex_t *mutexp;
  pthread_cond_t *condp;
  struct idb_queue *msgsp;

  memset(mboxp, 0, sizeof(mbox_t));
  condp = & mboxp->cond;
  pthread_cond_init(condp, NULL);
  mutexp = & mboxp->mutex;
  pthread_mutex_init(mutexp, NULL);
  msgsp = & mboxp->msgs;
  mboxp->nmsgs = 0;
  mboxp->nclients = 0;
  Yap_init_tqueue(msgsp);
  // match at the end, when everything is built.
  mboxp->name = namet;
  mboxp->open = true;
  return true;
}

static bool
mboxDestroy( mbox_t *mboxp USES_REGS )
{
  pthread_mutex_t *mutexp = &mboxp->mutex;
  pthread_cond_t *condp = &mboxp->cond;
  struct idb_queue *msgsp = &mboxp->msgs;
  mboxp->open = false;
  if (mboxp->nclients == 0 ) {
      pthread_cond_destroy(condp);
      pthread_mutex_destroy(mutexp);
      Yap_destroy_tqueue(msgsp PASS_REGS);
      // at this point, there is nothing left to unlock!
      return true;
  } else {
      /* we have clients in the mailbox, try to wake them up one by one */
      pthread_cond_broadcast(condp);
      pthread_mutex_unlock(mutexp);
      return true;
  }
}

static bool
mboxSend( mbox_t *mboxp, Term t USES_REGS )
{
  pthread_mutex_t *mutexp = &mboxp->mutex;
  pthread_cond_t *condp = &mboxp->cond;
  struct idb_queue *msgsp = &mboxp->msgs;

  if (!mboxp->open) {
      // oops, dead mailbox
      return false;
  }
  Yap_enqueue_tqueue(msgsp, t PASS_REGS);
  // printf("+   (%d) %d/%d\n", worker_id,mboxp->nclients, mboxp->nmsgs);
  mboxp->nmsgs++;
  pthread_cond_broadcast(condp);
  pthread_mutex_unlock(mutexp);
  return true;
}

static bool
mboxReceive( mbox_t *mboxp, Term t USES_REGS )
{
  pthread_mutex_t *mutexp = &mboxp->mutex;
  pthread_cond_t *condp = &mboxp->cond;
  struct idb_queue *msgsp = &mboxp->msgs;
  bool rc; 

  if (!mboxp->open){
    return false; 	// don't try to read if someone else already closed down...
  }
  mboxp->nclients++;
  do {
      rc = mboxp->nmsgs && Yap_dequeue_tqueue(msgsp, t, false,  true PASS_REGS);
      if (rc) {
	mboxp->nclients--;
	mboxp->nmsgs--;
	//printf("-   (%d) %d/%d\n", worker_id,mboxp->nclients, mboxp->nmsgs);
	//	Yap_do_low_level_trace=1;
	pthread_mutex_unlock(mutexp);
	return true;
      } else if (!mboxp->open) {
	//printf("o   (%d)\n", worker_id);
	mboxp->nclients--;
	if (!mboxp->nclients) {// release
	    pthread_cond_destroy(condp);
	      pthread_mutex_destroy(mutexp);
	      Yap_destroy_tqueue(msgsp PASS_REGS);
	      // at this point, there is nothing left to unlock!
	} else {
	  pthread_cond_broadcast(condp);
	  pthread_mutex_unlock(mutexp);
	}
	return false;
      } else {
	  pthread_cond_wait(condp, mutexp);
      }
  } while (!rc);
  return rc;
}

static bool
mboxPeek( mbox_t *mboxp, Term t USES_REGS )
{
  pthread_mutex_t *mutexp = &mboxp->mutex;
  struct idb_queue *msgsp = &mboxp->msgs;
  bool rc = Yap_dequeue_tqueue(msgsp, t, false,  false PASS_REGS);
  pthread_mutex_unlock(mutexp);
  return rc;
}

static int
store_specs(int new_worker_id, UInt ssize, UInt tsize, UInt sysize, Term tgoal, Term tdetach, Term texit)
{
  CACHE_REGS
  UInt pm;	/* memory to be requested         */
  Term tmod;

  if (tsize < MinTrailSpace)
    tsize = MinTrailSpace;
  if (ssize < MinStackSpace)
    ssize = MinStackSpace;
  REMOTE_ThreadHandle(new_worker_id).ssize = ssize;
  REMOTE_ThreadHandle(new_worker_id).tsize = tsize;
  REMOTE_ThreadHandle(new_worker_id).sysize = sysize;

  if ((REGSTORE *)pthread_getspecific(Yap_yaamregs_key)) {
    REMOTE_c_input_stream(new_worker_id) = LOCAL_c_input_stream;
    REMOTE_c_output_stream(new_worker_id) = LOCAL_c_output_stream;
    REMOTE_c_error_stream(new_worker_id) = LOCAL_c_error_stream;
  } else {
    // thread is created by a thread that has never run Prolog
    REMOTE_c_input_stream(new_worker_id) = REMOTE_c_input_stream(0);
    REMOTE_c_output_stream(new_worker_id) = REMOTE_c_output_stream(0);
    REMOTE_c_error_stream(new_worker_id) = REMOTE_c_error_stream(0);
  }
  pm = (ssize + tsize)*K1;
  if (!(REMOTE_ThreadHandle(new_worker_id).stack_address = malloc(pm))) {
    return FALSE;
  }
  REMOTE_ThreadHandle(new_worker_id).tgoal =
    Yap_StoreTermInDB(Deref(tgoal), 7);
      
  if (CurrentModule) {
    REMOTE_ThreadHandle(new_worker_id).cmod =
      CurrentModule;
  } else {
    REMOTE_ThreadHandle(new_worker_id).cmod = USER_MODULE;
  }
  tdetach = Deref(tdetach);
  if (IsVarTerm(tdetach)){
    REMOTE_ThreadHandle(new_worker_id).tdetach =  
      MkAtomTerm(AtomFalse);
  } else {
    REMOTE_ThreadHandle(new_worker_id).tdetach = 
      tdetach;
  }
  tmod = CurrentModule;
  texit = Yap_StripModule(Deref(texit), &tmod);
  if (IsAtomTerm(tmod)) {
      REMOTE_ThreadHandle(new_worker_id).texit_mod = tmod;
  } else {
     Yap_Error(TYPE_ERROR_ATOM,tmod,"module in exit call should be an atom");
  }
  REMOTE_ThreadHandle(new_worker_id).texit =
    Yap_StoreTermInDB(texit,7);
  REMOTE_ThreadHandle(new_worker_id).local_preds =
    NULL;
  REMOTE_ThreadHandle(new_worker_id).start_of_timesp =
    NULL;
  REMOTE_ThreadHandle(new_worker_id).last_timep =
    NULL;
  REMOTE_ScratchPad(new_worker_id).ptr =
    NULL;
  // reset arena info
  REMOTE_GlobalArena(new_worker_id) =0;
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
  REMOTE_Signals(wid) = 0L;
  // must be done before relessing the memory used to store 
  // thread local time.
  if (!always_die) {
    /* called by thread itself */
    GLOBAL_ThreadsTotalTime += Yap_cputime();
  }
  if (REMOTE_ScratchPad(wid).ptr)
    free(REMOTE_ScratchPad(wid).ptr);
  REMOTE_PL_local_data_p(wid)->reg_cache =
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
  GLOBAL_NOfThreads--;
  UNLOCK(GLOBAL_ThreadHandlesLock);
  MUTEX_LOCK(&(REMOTE_ThreadHandle(wid).tlock));
  if (REMOTE_ThreadHandle(wid).tdetach == MkAtomTerm(AtomTrue) ||
      always_die) {
    REMOTE_ThreadHandle(wid).zombie = FALSE;
    REMOTE_ThreadHandle(wid).in_use = FALSE;
  }
  MUTEX_UNLOCK(&(REMOTE_ThreadHandle(wid).tlock));
}

static void
thread_die(int wid, int always_die)
{
  kill_thread_engine(wid, always_die);
}

static int
setup_engine(int myworker_id, int init_thread)
{
  CACHE_REGS
  REGSTORE *standard_regs;
  
  set_system_thread_id( myworker_id, NULL );
  standard_regs = (REGSTORE *)calloc(1,sizeof(REGSTORE));
  if (!standard_regs)
    return FALSE;
  regcache = standard_regs;
  /* create the YAAM descriptor */
  REMOTE_ThreadHandle(myworker_id).default_yaam_regs = standard_regs;
  REMOTE_ThreadHandle(myworker_id).current_yaam_regs = standard_regs;
  REMOTE_PL_local_data_p(myworker_id)->reg_cache = standard_regs;
  Yap_InitExStacks(myworker_id, REMOTE_ThreadHandle(myworker_id).tsize, REMOTE_ThreadHandle(myworker_id).ssize);
  REMOTE_SourceModule(myworker_id) = CurrentModule = REMOTE_ThreadHandle(myworker_id).cmod;
  // create a mbox
  mboxCreate( MkIntTerm(myworker_id), &REMOTE_ThreadHandle(myworker_id).mbox_handle PASS_REGS );
  Yap_InitTime( myworker_id );
  Yap_InitYaamRegs( myworker_id );
  REFRESH_CACHE_REGS
  Yap_ReleasePreAllocCodeSpace(Yap_PreAllocCodeSpace());
  /* I exist */
  GLOBAL_NOfThreadsCreated++;
  GLOBAL_NOfThreads++;
  MUTEX_UNLOCK(&(REMOTE_ThreadHandle(myworker_id).tlock));  
#ifdef TABLING
  new_dependency_frame(REMOTE_top_dep_fr(myworker_id), FALSE, NULL, NULL, B, NULL, FALSE, NULL);  /* same as in Yap_init_root_frames() */
#endif /* TABLING */
  return TRUE;
}

static void
start_thread(int myworker_id)
{
  CACHE_REGS
  pthread_setspecific(Yap_yaamregs_key, (void *)REMOTE_ThreadHandle(myworker_id).default_yaam_regs);
  REFRESH_CACHE_REGS;
  worker_id = myworker_id;
  LOCAL = REMOTE(myworker_id);
}

static void *
thread_run(void *widp)
{
  CACHE_REGS
  Term tgoal, t;
  Term tgs[2];
  int myworker_id = *((int *)widp); 
#ifdef OUTPUT_THREADS_TABLING
  char thread_name[25];
  char filename[YAP_FILENAME_MAX]; 

  sprintf(thread_name, "/thread_output_%d", myworker_id);
  strcpy(filename, YAP_BINDIR);
  strncat(filename, thread_name, 25);
  REMOTE_thread_output(myworker_id) = fopen(filename, "w");
#endif /* OUTPUT_THREADS_TABLING */
  start_thread(myworker_id);
  REFRESH_CACHE_REGS;
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
#ifdef TABLING
  {
    tab_ent_ptr tab_ent;

    tab_ent = GLOBAL_root_tab_ent;
    while (tab_ent) {
      abolish_table(tab_ent);
      tab_ent = TabEnt_next(tab_ent);
    }
    FREE_DEPENDENCY_FRAME(REMOTE_top_dep_fr(worker_id));
    REMOTE_top_dep_fr(worker_id) = NULL;
#ifdef USE_PAGES_MALLOC
    DETACH_PAGES(_pages_void);
#endif /* USE_PAGES_MALLOC */
    DETACH_PAGES(_pages_tab_ent);
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
    DETACH_PAGES(_pages_sg_ent);
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
    DETACH_PAGES(_pages_sg_fr);
    DETACH_PAGES(_pages_dep_fr);
    DETACH_PAGES(_pages_sg_node);
    DETACH_PAGES(_pages_sg_hash);
    DETACH_PAGES(_pages_ans_node);
    DETACH_PAGES(_pages_ans_hash);
#if defined(THREADS_FULL_SHARING)
    DETACH_PAGES(_pages_ans_ref_node);
#endif /* THREADS_FULL_SHARING */
    DETACH_PAGES(_pages_gt_node);
    DETACH_PAGES(_pages_gt_hash);
#ifdef OUTPUT_THREADS_TABLING 
    fclose(LOCAL_thread_output);
#endif /* OUTPUT_THREADS_TABLING */

  }
#endif /* TABLING */
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
init_thread_engine(int new_worker_id, UInt ssize, UInt tsize, UInt sysize, Term tgoal, Term tdetach, Term texit)
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
  int new_worker_id = IntegerOfTerm(Deref(ARG7)),
    owid = worker_id;
  
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
  if (!init_thread_engine(new_worker_id, ssize, tsize, sysize, ARG1, ARG5, ARG6))
    return FALSE;
  //REMOTE_ThreadHandle(new_worker_id).pthread_handle = 0L;
  REMOTE_ThreadHandle(new_worker_id).id = new_worker_id;
  REMOTE_ThreadHandle(new_worker_id).ref_count = 1;
  setup_engine(new_worker_id, FALSE);
  if ((REMOTE_ThreadHandle(new_worker_id).ret = pthread_create(&REMOTE_ThreadHandle(new_worker_id).pthread_handle, NULL, thread_run, (void *)(&(REMOTE_ThreadHandle(new_worker_id).id)))) == 0) {
    pthread_setspecific(Yap_yaamregs_key, (const void *)REMOTE_ThreadHandle(owid).current_yaam_regs);
    /* wait until the client is initialised */
    return TRUE;
  }
  pthread_setspecific(Yap_yaamregs_key, (const void *)REMOTE_ThreadHandle(owid).current_yaam_regs);
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
  if (Yap_has_signal( YAP_ITI_SIGNAL )) {
    return FALSE;
  }
  //  fprintf(stderr," -- %d\n", worker_id); 
  LOCAL_ThreadHandle.in_use = FALSE;
  LOCAL_ThreadHandle.zombie = TRUE;
  MUTEX_UNLOCK(&(LOCAL_ThreadHandle.tlock));
  return Yap_unify(MkIntegerTerm(worker_id), ARG1);
}

static Int
p_thread_status_lock( USES_REGS1 )
{
  /* make sure the lock is available */
  if (pthread_getspecific(Yap_yaamregs_key) == NULL)
    return FALSE;
  MUTEX_LOCK(&(LOCAL_ThreadHandle.tlock_status));
  return Yap_unify(MkIntegerTerm(worker_id), ARG1);
}

static Int
p_thread_status_unlock( USES_REGS1 )
{
  /* make sure the lock is available */
  if (pthread_getspecific(Yap_yaamregs_key) == NULL)
    return FALSE;
  MUTEX_UNLOCK(&(LOCAL_ThreadHandle.tlock_status));
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
Yap_thread_create_engine(YAP_thread_attr *ops)
{
  YAP_thread_attr opsv;
  int new_id = allocate_new_tid();
  Term t = TermNil;

  /* 
     ok, this creates a problem, because we are initializing an engine from
     some "empty" thread. 
     We need first to fool the thread into believing it is the main thread
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
  if (!pthread_equal(pthread_self() , GLOBAL_master_thread) ) {
    /* we are worker_id 0 for now, lock master thread so that no one messes with us */ 
    pthread_setspecific(Yap_yaamregs_key, (const void *)&Yap_standard_regs);
    MUTEX_LOCK(&(REMOTE_ThreadHandle(0).tlock));
  }
  if (!init_thread_engine(new_id, ops->ssize, ops->tsize, ops->sysize, t, t, (ops->egoal)))
    return -1;
  //REMOTE_ThreadHandle(new_id).pthread_handle = 0L;
  REMOTE_ThreadHandle(new_id).id = new_id;
  REMOTE_ThreadHandle(new_id).ref_count = 0;
  if (!setup_engine(new_id, FALSE))
    return -1;
  if (!pthread_equal(pthread_self(), GLOBAL_master_thread)) {
    pthread_setspecific(Yap_yaamregs_key, NULL);
    MUTEX_UNLOCK(&(REMOTE_ThreadHandle(0).tlock));
  }
  return new_id;
}

Int
Yap_thread_attach_engine(int wid)
{
  /* 
     already locked
     MUTEX_LOCK(&(REMOTE_ThreadHandle(wid).tlock));
  */
  if (REMOTE_ThreadHandle(wid).ref_count ) {
    REMOTE_ThreadHandle(wid).ref_count++;
    REMOTE_ThreadHandle(wid).pthread_handle = pthread_self();
    set_system_thread_id(wid, SWI_thread_info(wid, NULL));
    MUTEX_UNLOCK(&(REMOTE_ThreadHandle(wid).tlock));
    return TRUE;
  }
  REMOTE_ThreadHandle(wid).pthread_handle = pthread_self();
  set_system_thread_id(wid, SWI_thread_info(wid, NULL));
  REMOTE_ThreadHandle(wid).ref_count++;
  pthread_setspecific(Yap_yaamregs_key, (const void *)REMOTE_ThreadHandle(wid).current_yaam_regs);
  MUTEX_UNLOCK(&(REMOTE_ThreadHandle(wid).tlock));
  return TRUE;
}

Int
Yap_thread_detach_engine(int wid)
{
  MUTEX_LOCK(&(REMOTE_ThreadHandle(wid).tlock));
  //REMOTE_ThreadHandle(wid).pthread_handle = 0;
  REMOTE_ThreadHandle(wid).ref_count--;
  pthread_setspecific(Yap_yaamregs_key, NULL);
  MUTEX_UNLOCK(&(REMOTE_ThreadHandle(wid).tlock));
  return TRUE;
}

Int
Yap_thread_destroy_engine(int wid)
{
  MUTEX_LOCK(&(REMOTE_ThreadHandle(wid).tlock));
  if (REMOTE_ThreadHandle(wid).ref_count == 0) {
    kill_thread_engine(wid, TRUE);
    return TRUE;
  } else {
    MUTEX_UNLOCK(&(REMOTE_ThreadHandle(wid).tlock));
    return FALSE;
  }
}


static Int
p_thread_join( USES_REGS1 )
{
  Int tid = IntegerOfTerm(Deref(ARG1));
  pthread_t thread;

  MUTEX_LOCK(&(REMOTE_ThreadHandle(tid).tlock));
  if (!(REMOTE_ThreadHandle(tid).in_use ||
	REMOTE_ThreadHandle(tid).zombie)) {
    // he's dead, jim
    MUTEX_UNLOCK(&(REMOTE_ThreadHandle(tid).tlock));
    return FALSE;
  }
  if (!REMOTE_ThreadHandle(tid).tdetach == MkAtomTerm(AtomTrue)) {
    MUTEX_UNLOCK(&(REMOTE_ThreadHandle(tid).tlock));
    return FALSE;
  }
  thread = REMOTE_ThreadHandle(tid).pthread_handle;
  MUTEX_UNLOCK(&(REMOTE_ThreadHandle(tid).tlock));
  /* make sure this lock is accessible */
  if (pthread_join(thread, NULL) < 0) {
    /* ERROR */
    return FALSE;
  }
  return TRUE;
}

static Int
p_thread_destroy( USES_REGS1 )
{
  Int tid = IntegerOfTerm(Deref(ARG1));

  MUTEX_LOCK(&(REMOTE_ThreadHandle(tid).tlock));
  REMOTE_ThreadHandle(tid).zombie = FALSE;
  REMOTE_ThreadHandle(tid).in_use = FALSE;
  MUTEX_UNLOCK(&(REMOTE_ThreadHandle(tid).tlock));
  return TRUE;
}

static Int
p_thread_detach( USES_REGS1 )
{
  Int tid = IntegerOfTerm(Deref(ARG1));
  MUTEX_LOCK(&(REMOTE_ThreadHandle(tid).tlock));
  if (pthread_detach(REMOTE_ThreadHandle(tid).pthread_handle) < 0) {
    /* ERROR */
    MUTEX_UNLOCK(&(REMOTE_ThreadHandle(tid).tlock));
    return FALSE;
  }
  REMOTE_ThreadHandle(tid).tdetach = 
    MkAtomTerm(AtomTrue);
  MUTEX_UNLOCK(&(REMOTE_ThreadHandle(tid).tlock));
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
  int newc;
#if HAVE_PTHREAD_GETCONCURRENCY
int cur;
#endif


  if (IsVarTerm(tnew)) {
    newc = 0;
  } else if (IsIntegerTerm(tnew)) {
    newc = IntegerOfTerm(tnew);
  } else {
    Yap_Error(TYPE_ERROR_INTEGER,tnew,"thread_set_concurrency/2");
    return(FALSE);
  }
#if HAVE_PTHREAD_GETCONCURRENCY
  cur = MkIntegerTerm(pthread_getconcurrency());
  if (pthread_setconcurrency(newc) != 0) {
    return FALSE;
  }
  return Yap_unify(ARG1, MkIntegerTerm(cur));
#else
  return FALSE;
#endif
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
#if defined(HAVE_PTHREAD_MUTEXATTR_SETKIND_NP) && !defined(__MINGW32__)
  extern int pthread_mutexattr_setkind_np(pthread_mutexattr_t *attr, int kind);
#endif

  mutp = (SWIMutex *)Yap_AllocCodeSpace(sizeof(SWIMutex));
  if (mutp == NULL) {
    return FALSE;
  }
  pthread_mutexattr_init(&mat);
#if defined(HAVE_PTHREAD_MUTEXATTR_SETKIND_NP)  && !defined(__MINGW32__)
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

 #if DEBUG_LOCKS
   MUTEX_LOCK(&mut->m);
 #else
   if (MUTEX_LOCK(&mut->m) < 0)
     return FALSE;
 #endif
   mut->owners++;
   mut->tid_own = worker_id;
   return TRUE;
 }

 static Int
 p_trylock_mutex( USES_REGS1 )
 {
   SWIMutex *mut = (SWIMutex*)IntegerOfTerm(Deref(ARG1));

   if (MUTEX_TRYLOCK(&mut->m) == EBUSY)
     return FALSE;
   mut->owners++;
   mut->tid_own = worker_id;
   return TRUE;
 }

 static Int
 p_unlock_mutex( USES_REGS1 )
 {
   SWIMutex *mut = (SWIMutex*)IntegerOfTerm(Deref(ARG1));

 #if DEBUG_LOCKS
   MUTEX_UNLOCK(&mut->m);
 #else
   if (MUTEX_UNLOCK(&mut->m) < 0)
     return FALSE;
 #endif
   mut->owners--;
   return TRUE;
 }

 static Int
 p_with_mutex( USES_REGS1 )
 {
   SWIMutex *mut;
   Term t1 = Deref(ARG1), excep;
   Int rc = FALSE;
   Int creeping = Yap_get_signal(YAP_CREEP_SIGNAL);
   PredEntry *pe;
   Term tm = CurrentModule;
   Term tg = Deref(ARG2);

   if (IsVarTerm(t1)) {
       p_new_mutex( PASS_REGS1 );
       t1 = Deref(ARG1);
   }
   mut = (SWIMutex*)IntegerOfTerm(t1);
   if (!p_lock_mutex( PASS_REGS1 )) {
       return FALSE;
   }

   tg = Yap_StripModule(tg, &tm);
   if (IsVarTerm(tg)) {
      Yap_Error(INSTANTIATION_ERROR, ARG2, "with_mutex/2");
      goto end;
   } else if (IsApplTerm(tg)) {
     register Functor f = FunctorOfTerm(tg);
     register CELL *pt;
     size_t i, arity;

    f = FunctorOfTerm(tg);
     if (IsExtensionFunctor(f)) {
       Yap_Error(TYPE_ERROR_CALLABLE, tg, "with_mutex/2");
       goto end;
     }
     arity = ArityOfFunctor(f);
     if (arity > MaxTemps) {
       Yap_Error(TYPE_ERROR_CALLABLE, tg, "with_mutex/2");
       goto end;
     }
     pe = RepPredProp(PredPropByFunc(f, tm));
     pt = RepAppl(tg)+1;
     for (i= 0; i < arity; i++ )
       XREGS[i+1] = pt[i];
   } else if (IsAtomTerm(tg)) {
       pe = RepPredProp(PredPropByAtom(AtomOfTerm(tg), tm));
   } else if (IsPairTerm(tg)) {
     register CELL *pt;
     Functor f;

     f = FunctorDot;
     pe = RepPredProp(PredPropByFunc(f, tm));
     pt = RepPair(tg);
     XREGS[1] = pt[0];
     XREGS[2] = pt[1];
     } else {
	      Yap_Error(TYPE_ERROR_CALLABLE, tg, "with_mutex/2");
	       goto end;
     }
   if (
       pe->OpcodeOfPred != FAIL_OPCODE &&
       Yap_execute_pred(pe, NULL PASS_REGS) ) {
       rc = TRUE;
   }
   end:
   ARG1 = MkIntegerTerm((Int)mut);
   excep = Yap_GetException();
   p_unlock_mutex( PASS_REGS1 );
   if (creeping) {
       Yap_signal( YAP_CREEP_SIGNAL );
   } else if ( excep != 0) {
       return Yap_JumpToEnv(excep);
   }
   return rc;
 }


 static Int
 p_with_with_mutex( USES_REGS1 )
 {
   if (GLOBAL_WithMutex == NULL) {
       p_new_mutex( PASS_REGS1 );
       GLOBAL_WithMutex = (SWIMutex*)IntegerOfTerm(Deref(ARG1));
   } else {
       ARG1 = MkIntegerTerm((Int)GLOBAL_WithMutex);
   }
   return p_lock_mutex( PASS_REGS1 );
 }

 static Int
 p_unlock_with_mutex( USES_REGS1 )
 {
   ARG1 = MkIntegerTerm((Int)GLOBAL_WithMutex);
   return p_unlock_mutex( PASS_REGS1 );
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

 typedef struct {
   UInt indx;
   mbox_t mbox;
 } counted_mbox;

 static Int
 p_mbox_create( USES_REGS1 )
 {
   Term namet = Deref(ARG1);
   mbox_t* mboxp = GLOBAL_named_mboxes;

   if (IsVarTerm(namet)) {
       AtomEntry *ae;
       int new;
       mbox_t mbox;

       ae = Yap_lookupBlob(&mbox, sizeof(mbox), &PL_Message_Queue, &new);
       namet = MkAtomTerm(RepAtom(ae));
       mboxp = (mbox_t *)(ae->rep.blob[0].data);
      Yap_unify(ARG1, namet);
      LOCK(GLOBAL_mboxq_lock);
   } else if (IsAtomTerm(namet)) {
       LOCK(GLOBAL_mboxq_lock);
       while( mboxp && mboxp->name != namet)
	 mboxp = mboxp->next;
       if (mboxp) {
	   UNLOCK(GLOBAL_mboxq_lock);
	   return FALSE;
       }
       mboxp = (mbox_t *)Yap_AllocCodeSpace(sizeof(mbox_t));
       if (mboxp == NULL) {
	   UNLOCK(GLOBAL_mboxq_lock);
	   return FALSE;
       }
       // global mbox, for now we'll just insert in list
       mboxp->next = GLOBAL_named_mboxes;
       GLOBAL_named_mboxes = mboxp;
   }
   bool rc = mboxCreate( namet, mboxp PASS_REGS );
   UNLOCK(GLOBAL_mboxq_lock);
   return rc;
 }

 static Int
 p_mbox_destroy( USES_REGS1 )
 {
   Term namet = Deref(ARG1);
   mbox_t* mboxp = GLOBAL_named_mboxes, *prevp;

   if (IsVarTerm(namet) )
      return FALSE;
   if (IsIntTerm(namet) ) {
      return FALSE;
   }
   LOCK(GLOBAL_mboxq_lock);
   prevp = NULL;
   while( mboxp && mboxp->name != namet) {
       prevp = mboxp;
     mboxp = mboxp->next;
   }
   if (!mboxp) {
       UNLOCK(GLOBAL_mboxq_lock);
     return FALSE;
   }
   if (mboxp == GLOBAL_named_mboxes) {
     GLOBAL_named_mboxes = mboxp->next;
   } else {
       prevp->next = mboxp->next;
   }
   UNLOCK(GLOBAL_mboxq_lock);
   mboxDestroy(mboxp PASS_REGS);
   Yap_FreeCodeSpace( (char *)mboxp );
   return TRUE;
 }

 static mbox_t*
 getMbox(Term t)
 {
   mbox_t* mboxp;

   if (IsAtomTerm(t=Deref(t))) {
     Atom at = AtomOfTerm(t);
     LOCK(GLOBAL_mboxq_lock);
     if (IsBlob(at)) {
       mboxp = (mbox_t *)(RepAtom(at)->rep.blob[0].data);
     } else {
       mboxp = GLOBAL_named_mboxes;
       while( mboxp && mboxp->name != t) {
	   mboxp = mboxp->next;
       }
     }
     if (!mboxp->open)
       mboxp = NULL;
     if (mboxp) {
	 pthread_mutex_lock(& mboxp->mutex);
     }
     UNLOCK(GLOBAL_mboxq_lock);
   } else if (IsIntTerm(t)) {
       int wid = IntOfTerm(t);
       if (REMOTE(wid) &&
	   (REMOTE_ThreadHandle(wid).in_use || REMOTE_ThreadHandle(wid).zombie))
       {
	 return &REMOTE_ThreadHandle(wid).mbox_handle;
       } else {
	  return NULL;
       }
       if (!mboxp->open)
	 mboxp = NULL;
       if (mboxp) {
	   pthread_mutex_lock(& mboxp->mutex);
       }
   } else {
       return NULL;
   }
   return mboxp;
 }


 static Int
 p_mbox_send( USES_REGS1 )
 {
   Term namet = Deref(ARG1);
   mbox_t* mboxp = getMbox(namet) ;

   if (!mboxp)
     return FALSE;
   return mboxSend(mboxp, Deref(ARG2) PASS_REGS);
 }

 static Int
 p_mbox_size( USES_REGS1 )
 {
   Term namet = Deref(ARG1);
   mbox_t* mboxp = getMbox(namet) ;

   if (!mboxp)
     return FALSE;
   return Yap_unify( ARG2, MkIntTerm(mboxp->nmsgs));
 }


 static Int
 p_mbox_receive( USES_REGS1 )
 {
   Term namet = Deref(ARG1);
    mbox_t* mboxp = getMbox(namet) ;

    if (!mboxp)
       return FALSE;
   return mboxReceive(mboxp, Deref(ARG2) PASS_REGS);
 }


 static Int
 p_mbox_peek( USES_REGS1 )
 {
   Term namet = Deref(ARG1);
    mbox_t* mboxp = getMbox(namet) ;

    if (!mboxp)
       return FALSE;
   return mboxPeek(mboxp, Deref(ARG2) PASS_REGS);
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
 v  return TRUE;
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

   MUTEX_LOCK(&(REMOTE_ThreadHandle(tid).tlock));
   if (REMOTE(tid) &&
       (REMOTE_ThreadHandle(tid).in_use || REMOTE_ThreadHandle(tid).zombie)) {
     status &= Yap_unify(ARG2,MkIntegerTerm(REMOTE_ThreadHandle(tid).ssize));
     status &= Yap_unify(ARG3,MkIntegerTerm(REMOTE_ThreadHandle(tid).tsize));
     status &= Yap_unify(ARG4,MkIntegerTerm(REMOTE_ThreadHandle(tid).sysize));
     MUTEX_UNLOCK(&(REMOTE_ThreadHandle(tid).tlock));
     return status;
   }
   MUTEX_UNLOCK(&(REMOTE_ThreadHandle(tid).tlock));
   return FALSE;
 }

 static Int 
 p_thread_atexit( USES_REGS1 )
 {				/* '$thread_signal'(+P)	 */
   Term t;

   if (LOCAL_ThreadHandle.texit == NULL ||
       LOCAL_ThreadHandle.texit->Entry == MkAtomTerm(AtomTrue)) {
     return FALSE;
   }
   do {
     t = Yap_PopTermFromDB(LOCAL_ThreadHandle.texit);
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
   LOCAL_ThreadHandle.texit = NULL;
   return Yap_unify(ARG1, t) && Yap_unify(ARG2, LOCAL_ThreadHandle.texit_mod);
 }



 static Int 
 p_thread_signal( USES_REGS1 )
 {				/* '$thread_signal'(+P)	 */
   Int wid = IntegerOfTerm(Deref(ARG1));
   /* make sure the lock is available */
   MUTEX_LOCK(&(REMOTE_ThreadHandle(wid).tlock));
   if (!REMOTE_ThreadHandle(wid).in_use || 
       !REMOTE_ThreadHandle(wid).current_yaam_regs) {
     MUTEX_UNLOCK(&(REMOTE_ThreadHandle(wid).tlock));
    return TRUE;
   }
   Yap_external_signal( wid,  YAP_ITI_SIGNAL );
     MUTEX_UNLOCK(&(REMOTE_ThreadHandle(wid).tlock));
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
   MUTEX_LOCK(&(LOCAL_ThreadHandle.tlock));
   return Yap_unify(ARG1,MkIntegerTerm(worker_id));
 }

 static Int 
 p_thread_unlock( USES_REGS1 )
 {				/* '$thread_unlock'	 */
   Int wid = IntegerOfTerm(Deref(ARG1));
   MUTEX_UNLOCK(&(REMOTE_ThreadHandle(wid).tlock));
   return TRUE;
 }

 intptr_t
 system_thread_id(PL_thread_info_t *info)
 { if ( !info )
   { CACHE_REGS
     if ( LOCAL )
       info = SWI_thread_info(worker_id, NULL);
     else
       return -1;
   }
 #ifdef __linux__
   return info->pid;
 #else
 #ifdef __WINDOWS__
   return info->w32id;
 #else
   return (intptr_t)info->tid;
 #endif
 #endif
 }

 void 
 Yap_InitFirstWorkerThreadHandle(void)
 {
   CACHE_REGS
   set_system_thread_id(0, NULL);
   LOCAL_ThreadHandle.id = 0;
   LOCAL_ThreadHandle.in_use = TRUE;
   LOCAL_ThreadHandle.default_yaam_regs = 
     &Yap_standard_regs;
   LOCAL_ThreadHandle.current_yaam_regs = 
     &Yap_standard_regs;
   LOCAL_PL_local_data_p->reg_cache =
     &Yap_standard_regs;
   LOCAL_ThreadHandle.pthread_handle = pthread_self();
   pthread_mutex_init(&REMOTE_ThreadHandle(0).tlock, NULL);
   pthread_mutex_init(&REMOTE_ThreadHandle(0).tlock_status, NULL);
   LOCAL_ThreadHandle.tdetach = MkAtomTerm(AtomFalse);
   LOCAL_ThreadHandle.ref_count = 1;
 }

 FILE *debugf;

 void Yap_InitThreadPreds(void)
 {


   Yap_InitCPred("$no_threads", 0, p_no_threads, 0);
   Yap_InitCPred("$max_workers", 1, p_max_workers, 0);
   Yap_InitCPred("$max_threads", 1, p_max_threads, 0);
   Yap_InitCPred("$thread_new_tid", 1, p_thread_new_tid, 0);
   Yap_InitCPred("$create_thread", 7, p_create_thread, 0);
   Yap_InitCPred("$thread_self", 1, p_thread_self, SafePredFlag);
   Yap_InitCPred("$thread_status_lock", 1, p_thread_status_lock, SafePredFlag);
   Yap_InitCPred("$thread_status_unlock", 1, p_thread_status_unlock, SafePredFlag);
   Yap_InitCPred("$thread_zombie_self", 1, p_thread_zombie_self, SafePredFlag);
   Yap_InitCPred("$thread_join", 1, p_thread_join, 0);
   Yap_InitCPred("$thread_destroy", 1, p_thread_destroy, 0);
   Yap_InitCPred("thread_yield", 0, p_thread_yield, 0);
 /** @pred thread_yield 


 Voluntarily relinquish the processor.


 */
   Yap_InitCPred("$detach_thread", 1, p_thread_detach, 0);
   Yap_InitCPred("$thread_detached", 1, p_thread_detached, 0);
   Yap_InitCPred("$thread_detached", 2, p_thread_detached2, 0);
   Yap_InitCPred("$thread_exit", 0, p_thread_exit, 0);
   Yap_InitCPred("thread_setconcurrency", 2, p_thread_set_concurrency, 0);
 /** @pred thread_setconcurrency(+ _Old_, - _New_) 


 Determine the concurrency of the process, which is defined as the
 maximum number of concurrently active threads. `Active` here means
 they are using CPU time. This option is provided if the
 thread-implementation provides
 `pthread_setconcurrency()`. Solaris is a typical example of this
 family. On other systems this predicate unifies  _Old_ to 0 (zero)
 and succeeds silently.


 */
   Yap_InitCPred("$valid_thread", 1, p_valid_thread, 0);
   Yap_InitCPred("$new_mutex", 1, p_new_mutex, SafePredFlag);
   Yap_InitCPred("$destroy_mutex", 1, p_destroy_mutex, SafePredFlag);
   Yap_InitCPred("$lock_mutex", 1, p_lock_mutex, SafePredFlag);
   Yap_InitCPred("$trylock_mutex", 1, p_trylock_mutex, SafePredFlag);
   Yap_InitCPred("$unlock_mutex", 1, p_unlock_mutex, SafePredFlag);
   Yap_InitCPred("$with_mutex", 2, p_with_mutex, MetaPredFlag);
   Yap_InitCPred("$with_with_mutex", 1, p_with_with_mutex, 0);
   Yap_InitCPred("$unlock_with_mutex", 1, p_unlock_with_mutex, 0);
   Yap_InitCPred("$mutex_info", 3, p_mutex_info, SafePredFlag);
   Yap_InitCPred("$cond_create", 1, p_cond_create, SafePredFlag);
   Yap_InitCPred("$cond_destroy", 1, p_cond_destroy, SafePredFlag);
   Yap_InitCPred("$cond_signal", 1, p_cond_signal, SafePredFlag);
   Yap_InitCPred("$cond_broadcast", 1, p_cond_broadcast, SafePredFlag);
   Yap_InitCPred("$cond_wait", 2, p_cond_wait, SafePredFlag);
   Yap_InitCPred("$message_queue_create", 1, p_mbox_create, SafePredFlag);
   Yap_InitCPred("$message_queue_destroy", 1, p_mbox_destroy, SafePredFlag);
   Yap_InitCPred("$message_queue_send", 2, p_mbox_send, SafePredFlag);
   Yap_InitCPred("$message_queue_receive", 2, p_mbox_receive, SafePredFlag);
   Yap_InitCPred("$message_queue_size", 2, p_mbox_size, SafePredFlag);
   Yap_InitCPred("$message_queue_peek", 2, p_mbox_peek, SafePredFlag);
   Yap_InitCPred("$thread_stacks", 4, p_thread_stacks, SafePredFlag);
   Yap_InitCPred("$signal_thread", 1, p_thread_signal, SafePredFlag);
   Yap_InitCPred("$nof_threads", 1, p_nof_threads, SafePredFlag);
   Yap_InitCPred("$nof_threads_created", 1, p_nof_threads_created, SafePredFlag);
   Yap_InitCPred("$thread_sleep", 4, p_thread_sleep, SafePredFlag);
   Yap_InitCPred("$thread_runtime", 1, p_thread_runtime, SafePredFlag);
   Yap_InitCPred("$thread_self_lock", 1, p_thread_self_lock, SafePredFlag);
   Yap_InitCPred("$thread_run_at_exit", 2, p_thread_atexit, SafePredFlag);
   Yap_InitCPred("$thread_unlock", 1, p_thread_unlock, SafePredFlag);
 #if DEBUG_LOCKS||DEBUG_PE_LOCKS
   Yap_InitCPred("debug_locks", 0, p_debug_locks, SafePredFlag);
   Yap_InitCPred("nodebug_locks", 0, p_nodebug_locks, SafePredFlag);
 #endif
 }

 #else

 int
 Yap_NOfThreads(void) {
   // GLOBAL_ThreadHandlesLock is held
 #ifdef YAPOR
   return 2;
 #else
   return 1;
 #endif
 }


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

 static Int 
 p_new_mutex(void)
{				/* '$max_workers'(+P)	 */
  static int mutexes = 1;
  return Yap_unify(ARG1, MkIntegerTerm(mutexes++) );
}

 static Int
 p_with_mutex( USES_REGS1 )
 {
   Int mut;
   Term t1 = Deref(ARG1), excep;
   Int rc = FALSE;
   Int creeping = Yap_get_signal(YAP_CREEP_SIGNAL);
   PredEntry *pe;
   Term tm = CurrentModule;
   Term tg = Deref(ARG2);

   if (IsVarTerm(t1)) {
       p_new_mutex( PASS_REGS1 );
       t1 = Deref(ARG1);
       mut = IntOfTerm(t1);
   }

   tg = Yap_StripModule(tg, &tm);
   if (IsVarTerm(tg)) {
      Yap_Error(INSTANTIATION_ERROR, ARG2, "with_mutex/2");
      goto end;
   } else if (IsApplTerm(tg)) {
     register Functor f = FunctorOfTerm(tg);
     register CELL *pt;
     size_t i, arity;

    f = FunctorOfTerm(tg);
     if (IsExtensionFunctor(f)) {
       Yap_Error(TYPE_ERROR_CALLABLE, tg, "with_mutex/2");
       goto end;
     }
     arity = ArityOfFunctor(f);
     if (arity > MaxTemps) {
       Yap_Error(TYPE_ERROR_CALLABLE, tg, "with_mutex/2");
       goto end;
     }
     pe = RepPredProp(PredPropByFunc(f, tm));
     pt = RepAppl(tg)+1;
     for (i= 0; i < arity; i++ )
       XREGS[i+1] = pt[i];
   } else if (IsAtomTerm(tg)) {
       pe = RepPredProp(PredPropByAtom(AtomOfTerm(tg), tm));
   } else if (IsPairTerm(tg)) {
     register CELL *pt;
     Functor f;

     f = FunctorDot;
     pe = RepPredProp(PredPropByFunc(f, tm));
     pt = RepPair(tg);
     XREGS[1] = pt[0];
     XREGS[2] = pt[1];
     } else {
	      Yap_Error(TYPE_ERROR_CALLABLE, tg, "with_mutex/2");
	       goto end;
     }
   if (
       pe->OpcodeOfPred != FAIL_OPCODE &&
       Yap_execute_pred(pe, NULL PASS_REGS) ) {
       rc = TRUE;
   }
   end:
   ARG1 = MkIntegerTerm(mut);
   excep = Yap_GetException();
   if (creeping) {
       Yap_signal( YAP_CREEP_SIGNAL );
   } else if ( excep != 0) {
       return Yap_JumpToEnv(excep);
   }
   return rc;
 }

void 
Yap_InitFirstWorkerThreadHandle(void)
{
}

void Yap_InitThreadPreds(void)
{ 
  Yap_InitCPred("$with_mutex", 2, p_with_mutex, MetaPredFlag);
  Yap_InitCPred("$new_mutex", 1, p_new_mutex, SafePredFlag);
  Yap_InitCPred("$max_workers", 1, p_max_workers, 0);
  Yap_InitCPred("$thread_self", 1, p_thread_self, SafePredFlag);
  Yap_InitCPred("$no_threads", 0, p_no_threads, SafePredFlag);
  Yap_InitCPred("$max_threads", 1, p_max_threads, SafePredFlag);
  Yap_InitCPred("$nof_threads", 1, p_nof_threads, SafePredFlag);
  Yap_InitCPred("$nof_threads_created", 1, p_nof_threads_created, SafePredFlag);
  Yap_InitCPred("$thread_stacks", 4, p_thread_stacks, SafePredFlag);
  Yap_InitCPred("$thread_runtime", 1, p_thread_runtime, SafePredFlag);
  Yap_InitCPred("$thread_unlock", 1, p_thread_unlock, SafePredFlag);
#if DEBUG_LOCKS||DEBUG_PE_LOCKS
  Yap_InitCPred("debug_locks", 0, p_debug_locks, SafePredFlag);
  Yap_InitCPred("nodebug_locks", 0, p_nodebug_locks, SafePredFlag);
#endif
}


#endif /* THREADS */


/**
@}
*/
