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
#include "YapEval.h"
#include "yapio.h"
#include "YapBlobs.h"
#include <stdio.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_SYS_SYSCALL_H
#include <sys/syscall.h>
#endif
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */


blob_type_t PL_Message_Queue = {
  YAP_BLOB_MAGIC_B,
  PL_BLOB_UNIQUE | PL_BLOB_NOCOPY,
  "message_queue",
  0, // release
  0, // compare
  0, // write
  0 // acquire
};


#if DEBUG_LOCKS||DEBUG_PE_LOCKS

bool debug_locks = true, debug_pe_locks = true;
static Int p_debug_locks( USES_REGS1 ) { debug_pe_locks = 1; return TRUE; }

static Int p_nodebug_locks( USES_REGS1 ) { debug_locks = 0; debug_pe_locks = 0; return TRUE; }

#endif

#if THREADS

#include "threads.h"



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
//  if (REMOTE_TmpPred(wid).ptr)
//    free(REMOTE_TmpPred(wid).ptr);
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
  
  standard_regs = (REGSTORE *)calloc(1,sizeof(REGSTORE));
  if (!standard_regs)
    return FALSE;
  regcache = standard_regs;
  /* create the YAAM descriptor */
  REMOTE_ThreadHandle(myworker_id).default_yaam_regs = standard_regs;
  REMOTE_ThreadHandle(myworker_id).current_yaam_regs = standard_regs;
  Yap_InitExStacks(myworker_id, REMOTE_ThreadHandle(myworker_id).tsize, REMOTE_ThreadHandle(myworker_id).ssize);
  REMOTE_SourceModule(myworker_id) = CurrentModule = REMOTE_ThreadHandle(myworker_id).cmod;
  // create a mbox
  mboxCreate( MkIntTerm(myworker_id), &REMOTE_ThreadHandle(myworker_id).mbox_handle PASS_REGS );
  Yap_InitTime( myworker_id );
  Yap_InitYaamRegs( myworker_id, true] );
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
      if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
	LOCAL_Error_TYPE = YAP_NO_ERROR;
	if (!Yap_growglobal(NULL)) {
	  Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil, LOCAL_ErrorMessage);
	  thread_die(worker_id, FALSE);
	  return NULL;
	}
      } else {
	LOCAL_Error_TYPE = YAP_NO_ERROR;
	if (!Yap_growstack(LOCAL_ThreadHandle.tgoal->NOfCells*CellSize)) {
	  Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
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
    /* wait until the client is initialized */
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
    Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1, "%s in thread_sleep/1", strerror(errno));
#else
    Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1, "error %d in thread_sleep/1", errno);
#endif
    return FALSE;
  }
  return Yap_unify(ARG3,MkIntegerTerm(oreq.tv_sec)) &&
    Yap_unify(ARG4,MkIntegerTerm(oreq.tv_nsec));
#elif HAVE_SLEEP
  UInt rtime;
  if ((rtime = sleep(time)) < 0) {
#if HAVE_STRERROR
    Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1, "%s in thread_sleep/1", strerror(errno));
#else
    Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1, "error %d in thread_sleep/1", errno);
#endif
  }
  return Yap_unify(ARG3,MkIntegerTerm(rtime)) &&
    Yap_unify(ARG4,MkIntTerm(0L));
#else 
  Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1, "no support for thread_sleep/1 in this YAP configuration");
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
    MUTEX_UNLOCK(&(REMOTE_ThreadHandle(wid).tlock));
    return TRUE;
  }
  REMOTE_ThreadHandle(wid).pthread_handle = pthread_self();
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
#if HAVE_PTHREAD_GETCONCURRENCY
  int newc;  
  int cur;
  Term tnew = Deref(ARG2);  
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
  MutexEntry *alias;
  pthread_mutex_t m;
  UInt timestamp;
  struct swi_mutex *backbone; // chain of all mutexes
  struct swi_mutex *prev, *next; // chain of locked mutexes
} SWIMutex;

static SWIMutex *NewMutex(void) {
  SWIMutex* mutp;
  pthread_mutexattr_t mat;
#if defined(HAVE_PTHREAD_MUTEXATTR_SETKIND_NP) && !defined(__MINGW32__)
  extern int pthread_mutexattr_setkind_np(pthread_mutexattr_t *attr, int kind);
#endif

  LOCK(GLOBAL_MUT_ACCESS);
  mutp = GLOBAL_FreeMutexes;
  while (mutp) {
    if ((Int)(mutp->owners) < 0) {
      // just making sure
      break;
    }
    mutp = mutp->next;
  }
  if (mutp == NULL) {
    mutp = (SWIMutex *)Yap_AllocCodeSpace(sizeof(SWIMutex));
    if (mutp == NULL) {
      UNLOCK(GLOBAL_MUT_ACCESS);
      return NULL;
    } else {
      pthread_mutexattr_init(&mat);
      mutp->timestamp = 0;
#if defined(HAVE_PTHREAD_MUTEXATTR_SETKIND_NP)  && !defined(__MINGW32__)
      pthread_mutexattr_setkind_np(&mat, PTHREAD_MUTEX_RECURSIVE_NP);
#else
#ifdef HAVE_PTHREAD_MUTEXATTR_SETTYPE
      pthread_mutexattr_settype(&mat, PTHREAD_MUTEX_RECURSIVE);
#endif
#endif
      pthread_mutex_init(&mutp->m, &mat);
    }
    mutp->backbone  = GLOBAL_mutex_backbone;
    GLOBAL_mutex_backbone = mutp;
  } else {
    // reuse existing mutex
    mutp->timestamp++;
  }
  mutp->owners = 0;
  mutp->tid_own = 0;  
  mutp->alias = NIL;  
  UNLOCK(GLOBAL_MUT_ACCESS);
  return mutp;
}

#define MutexOfTerm(t) MutexOfTerm__(t PASS_REGS)

static SWIMutex *MutexOfTerm__(Term t USES_REGS){
  Term t1 = Deref(t);
  SWIMutex *mut = NULL;
  
  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "mutex operation");
    return NULL;
  } else if (IsApplTerm(t1) && FunctorOfTerm(t1) == FunctorMutex) {
    mut = AddressOfTerm(ArgOfTerm(1,t1));
    if ((Int)(mut->owners) < 0 ||
	IntegerOfTerm(ArgOfTerm(2,t1)) != mut->timestamp) {
      Yap_Error(EXISTENCE_ERROR_MUTEX,  t1, "mutex access");
      return NULL;
    }
  } else if (IsAtomTerm(t1)) {
    mut = Yap_GetMutexFromProp(AtomOfTerm(t1));
    if (!mut) {
      mut = NewMutex();
      if ( !Yap_PutAtomMutex( AtomOfTerm(t1), mut ) ) {
	return NULL;
      }
    }
  }
  return mut;
}

static Int
p_new_mutex( USES_REGS1 ){
  SWIMutex* mutp;
  Term t1;
  if (IsVarTerm((t1 = Deref(ARG1)))) {
    Term ts[2];
    
    if (!(mutp = NewMutex()))
      return FALSE;
    ts[0] = MkAddressTerm(mutp);    
    ts[1] = MkIntegerTerm(mutp->timestamp);    
    if (Yap_unify(ARG1, Yap_MkApplTerm(FunctorMutex, 2, ts) ) ) {
      return TRUE;
    }
    Yap_Error(UNINSTANTIATION_ERROR, t1, "mutex_create on an existing mutex");
    return FALSE;
  } else if(IsAtomTerm(t1)) {
    if (!(mutp = NewMutex()))
      return FALSE;
    return Yap_PutAtomMutex( AtomOfTerm(t1), mutp );
  } else if (IsApplTerm(t1)  && FunctorOfTerm(t1) == FunctorMutex) {
    Yap_Error(UNINSTANTIATION_ERROR, t1, "mutex_create on an existing mutex");
    return FALSE;
  }
  return FALSE;
}
  
/** @pred mutex_destroy(+ _MutexId_)
    
    Destroy a mutex.  After this call,  _MutexId_ becomes invalid and
    further references yield an `existence_error` exception. 
*/
static Int p_destroy_mutex( USES_REGS1 )
{
  SWIMutex *mut = MutexOfTerm(Deref(ARG1));
  if (!mut)
    return FALSE;  
  if (pthread_mutex_destroy(&mut->m) < 0)
     return FALSE;
  if (mut->alias) {
    mut->alias->Mutex  = NULL;
  }
  mut->owners = -1;
  mut->tid_own = -1;
  LOCK(GLOBAL_MUT_ACCESS);
  if (GLOBAL_FreeMutexes)
    mut->prev = GLOBAL_FreeMutexes->prev;
  else
    mut->prev = NULL;
  mut->next = GLOBAL_FreeMutexes;
  GLOBAL_FreeMutexes = mut;
  UNLOCK(GLOBAL_MUT_ACCESS);
  return TRUE;
}

static bool
LockMutex( SWIMutex *mut USES_REGS)
{
#if DEBUG_LOCKS
  MUTEX_LOCK(&mut->m);
#else
  if (MUTEX_LOCK(&mut->m) < 0)
    return FALSE;
#endif
  mut->owners++;
  mut->tid_own = worker_id;
  if (LOCAL_Mutexes)
    mut->prev = LOCAL_Mutexes->prev;
  else
    mut->prev = NULL;
  mut->next = LOCAL_Mutexes;
  LOCAL_Mutexes = NULL;
  return true;
}

static bool
UnLockMutex( SWIMutex *mut USES_REGS)
{
#if DEBUG_LOCKS
  MUTEX_UNLOCK(&mut->m);
#else
  if (MUTEX_UNLOCK(&mut->m) < 0)
    return FALSE;
#endif
  mut->owners--;
  if (mut->prev) {
    mut->prev->next = mut->next;
  } else {
    LOCAL_Mutexes = mut->next;
    if (mut->next)
      mut->next->prev = NULL;
  }
  if (mut->next)
    mut->next->prev = mut->prev;
  return true;
}

/** @pred mutex_lock(+ _MutexId_) 


    Lock the mutex.  Prolog mutexes are <em>recursive</em> mutexes: they
    can be locked multiple times by the same thread.  Only after unlocking
    it as many times as it is locked, the mutex becomes available for
    locking by other threads. If another thread has locked the mutex the
    calling thread is suspended until to mutex is unlocked.

    If  _MutexId_ is an atom, and there is no current mutex with that
    name, the mutex is created automatically using mutex_create/1.  This
    implies named mutexes need not be declared explicitly.

    Please note that locking and unlocking mutexes should be paired
    carefully. Especially make sure to unlock mutexes even if the protected
    code fails or raises an exception. For most common cases use
    with_mutex/2, which provides a safer way for handling Prolog-level
    mutexes.

 
*/
static Int
p_lock_mutex( USES_REGS1 )
{
  SWIMutex *mut = MutexOfTerm(Deref(ARG1));
  if (!mut || !LockMutex( mut PASS_REGS))
    return FALSE;
  return TRUE;
}

/** @pred mutex_trylock(+ _MutexId_) 


    As mutex_lock/1, but if the mutex is held by another thread, this
    predicates fails immediately.

 
*/
static Int
p_trylock_mutex( USES_REGS1 )
{
  SWIMutex *mut = MutexOfTerm(Deref(ARG1));
  if (!mut)
    return FALSE;
  
  if (MUTEX_TRYLOCK(&mut->m) == EBUSY)
    return FALSE;
  mut->owners++;
  mut->tid_own = worker_id;
  return TRUE;
}

/** @pred mutex_unlock(+ _MutexId_) 


    Unlock the mutex. This can only be called if the mutex is held by the
    calling thread. If this is not the case, a `permission_error`
    exception is raised.

 
*/
static Int
p_unlock_mutex( USES_REGS1 )
{
  SWIMutex *mut = MutexOfTerm(Deref(ARG1));
  if (!mut || !UnLockMutex( mut PASS_REGS))
    return FALSE;
  return TRUE;
}

/** @pred with_mutex(+ _MutexId_, : _Goal_) 


    Execute  _Goal_ while holding  _MutexId_.  If  _Goal_ leaves
    choicepoints, these are destroyed (as in once/1).  The mutex is unlocked
    regardless of whether  _Goal_ succeeds, fails or raises an exception.
    An exception thrown by  _Goal_ is re-thrown after the mutex has been
    successfully unlocked.  See also `mutex_create/2`.

    Although described in the thread-section, this predicate is also
    available in the single-threaded version, where it behaves simply as
    once/1.

 
*/

static Int
p_with_mutex( USES_REGS1 )
{
  Term excep;
  Int rc = FALSE;
  Int creeping = Yap_get_signal(YAP_CREEP_SIGNAL);
  PredEntry *pe;
  Term tm = CurrentModule;
  Term tg = Deref(ARG2);
  SWIMutex *mut = MutexOfTerm( ARG1 );

  if (!mut || !LockMutex(mut PASS_REGS)) {
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
      Yap_execute_pred(pe, NULL, true PASS_REGS) ) {
    rc = TRUE;
  }
 end:
  excep = Yap_GetException(LOCAL_ComiittedError);
  if ( !UnLockMutex(mut PASS_REGS) ) {
    return FALSE;c
  }
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
  SWIMutex *mut = MutexOfTerm(Deref(ARG1));
  if (!mut)
    return FALSE;

  return Yap_unify(ARG2, MkIntegerTerm(mut->owners)) &&
    Yap_unify(ARG3, MkIntegerTerm(mut->tid_own));
  return TRUE;
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
      if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
	LOCAL_Error_TYPE = YAP_NO_ERROR;
	if (!Yap_growglobal(NULL)) {
	  Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil, LOCAL_ErrorMessage);
	  thread_die(worker_id, FALSE);
	  return FALSE;
	}
      } else {
	LOCAL_Error_TYPE = YAP_NO_ERROR;
	if (!Yap_growstack(LOCAL_ThreadHandle.tgoal->NOfCells*CellSize)) {
	  Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
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
system_thread_id(void)
{
#if defined(__APPLE__)
  return      syscall(SYS_thread_selfid);
#elif HAVE_SYS_GETTID || defined(__APPLE__)
  return syscall( SYS_GETTID );
#elif HAVE_GETTID_SYSCALL
    return syscall(__NR_gettid);
#elif defined( HAVE_GETTID_MACRO )
    return gettid();
#elif  defined(__WINDOWS__)
    return GetCurrentThreadId();
#endif

}



void 
Yap_InitFirstWorkerThreadHandle(void)
{
  CACHE_REGS
  LOCAL_ThreadHandle.id = 0;
  LOCAL_ThreadHandle.in_use = TRUE;
  LOCAL_ThreadHandle.default_yaam_regs = 
    &Yap_standard_regs;
  LOCAL_ThreadHandle.current_yaam_regs = 
    &Yap_standard_regs;
  LOCAL_ThreadHandle.pthread_handle = pthread_self();
  pthread_mutex_init(&REMOTE_ThreadHandle(0).tlock, NULL);
  pthread_mutex_init(&REMOTE_ThreadHandle(0).tlock_status, NULL);
  LOCAL_ThreadHandle.tdetach = MkAtomTerm(AtomFalse);
  LOCAL_ThreadHandle.ref_count = 1;
}

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
  Yap_InitCPred("mutex_create", 1, p_new_mutex, SafePredFlag);
  Yap_InitCPred("mutex_destroy", 1, p_destroy_mutex, SafePredFlag);
  Yap_InitCPred("mutex_lock", 1, p_lock_mutex, SafePredFlag);
  Yap_InitCPred("mutex_trylock", 1, p_trylock_mutex, SafePredFlag);
  Yap_InitCPred("mutex_unlock", 1, p_unlock_mutex, SafePredFlag);
  Yap_InitCPred("with_mutex", 2, p_with_mutex, MetaPredFlag);
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
   Term t1 = Deref(ARG1);
   Int rc = FALSE;
   Int creeping = Yap_get_signal(YAP_CREEP_SIGNAL);
   PredEntry *pe;
   Term tm = CurrentModule;
   Term tg = Deref(ARG2);

   if (IsVarTerm(t1)) {
       p_new_mutex( PASS_REGS1 );
   }
   t1 = Deref(ARG1);
   mut = IntOfTerm(t1);
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
      Yap_execute_pred(pe, NULL, false PASS_REGS) ) {
    rc = TRUE;
  }
 end:
  ARG1 = MkIntegerTerm(mut);
  yap_error_descriptor_t *err = Yap_GetException();
  if (creeping) {
    Yap_signal( YAP_CREEP_SIGNAL );
  } else if ( err ) {
    LOCAL_ActiveError->errorNo = err->errorNo;
    return Yap_JumpToEnv();
  }
  return rc;
}

void 
Yap_InitFirstWorkerThreadHandle(void)
{
}

void Yap_InitThreadPreds(void)
{ 
  Yap_InitCPred("with_mutex", 2, p_with_mutex, MetaPredFlag);
  Yap_InitCPred("mutex_create", 1, p_new_mutex, SafePredFlag);
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
