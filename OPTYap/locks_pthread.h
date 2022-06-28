/************************************************************************
**                                                                     **
**                   The YapTab/YapOr/OPTYap systems                   **
**                                                                     **
** YapTab extends the Yap Prolog engine to support sequential tabling  **
** YapOr extends the Yap Prolog engine to support or-parallelism       **
** OPTYap extends the Yap Prolog engine to support or-parallel tabling **
**                                                                     **
**                                                                     **
**      Yap Prolog was developed at University of Porto, Portugal      **
**                                                                     **
************************************************************************/

/* **********************************************************************
**                      Atomic locks for PTHREADS                      **
************************************************************************/

#ifndef LOCK_PTHREAD_H

#define LOCK_PTHREAD_H 1

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600
#endif

#include <stdbool.h>

#include <pthread.h>

#include <assert.h>

typedef pthread_mutex_t lockvar;
typedef pthread_rwlock_t rwlock_t;
//#define DEBUG_RW_LOCKS 1
//#define DEBUG_LOCKS 1
#include <stdio.h>

typedef pthread_rwlock_t rwlock_t;

int Yap_ThreadID( void );
#define debugf ( stderr ? stderr : stdout )

#if DEBUG_LOCKS
extern bool debug_locks;

#define INIT_LOCK(LOCK_VAR)    (void)(fprintf(debugf, "[%d] %s:%d:%s INITLOCK(%p)\n",  Yap_ThreadID(),__FILE__, __LINE__, __FUNCTION__,&(LOCK_VAR))  && pthread_mutex_init(&(LOCK_VAR), NULL) )
#define DESTROY_LOCK(LOCK_VAR) (void)(fprintf(debugf, "[%d] %s:%d:%s DESTROYLOCK(%p)\n",  Yap_ThreadID(),__FILE__, __LINE__, __FUNCTION__,&(LOCK_VAR))  && pthread_mutex_destroy(&(LOCK_VAR)) )
#define TRY_LOCK(LOCK_VAR)     (void)(fprintf(debugf, "[%d] %s:%d:%s TRYLOCK(%p)\n",  Yap_ThreadID(),__FILE__, __LINE__, __FUNCTION__,&(LOCK_VAR))  && pthread_mutex_trylock(&(LOCK_VAR)) )
#define LOCK(LOCK_VAR)         (void)(fprintf(debugf, "[%d] %s:%d:%s LOCK(%p)\n",  Yap_ThreadID(),__FILE__, __LINE__, __FUNCTION__,&(LOCK_VAR))  && pthread_mutex_lock(&(LOCK_VAR)) )
#define UNLOCK(LOCK_VAR)       (void)(fprintf(debugf, "[%d] %s:%d:%s UNLOCK(%p)\n",  Yap_ThreadID(),__FILE__, __LINE__, __FUNCTION__,&(LOCK_VAR))  && pthread_mutex_unlock(&(LOCK_VAR)) )
#else
#define INIT_LOCK(LOCK_VAR)    pthread_mutex_init(&(LOCK_VAR), NULL)
#define DESTROY_LOCK(LOCK_VAR) pthread_mutex_destroy(&(LOCK_VAR))
#define TRY_LOCK(LOCK_VAR)     pthread_mutex_trylock(&(LOCK_VAR))
#define LOCK(LOCK_VAR)         pthread_mutex_lock(&(LOCK_VAR))
#define UNLOCK(LOCK_VAR)       pthread_mutex_unlock(&(LOCK_VAR))
#endif

static inline bool
xIS_LOCKED(pthread_mutex_t *LOCK_VAR) {
  if (pthread_mutex_trylock(LOCK_VAR) == 0) {
    pthread_mutex_unlock(LOCK_VAR);
    return true;
  }
  return false;
}
static inline bool
xIS_UNLOCKED(pthread_mutex_t *LOCK_VAR) {
  if (pthread_mutex_trylock(LOCK_VAR) == 0) {
    pthread_mutex_unlock(LOCK_VAR);
    return false;
  }
  return true;
}

#define IS_LOCKED(LOCK_VAR)    xIS_LOCKED(&(LOCK_VAR))
#define IS_UNLOCKED(LOCK_VAR)  xIS_UNLOCKED(&(LOCK_VAR))

#if DEBUG_RW_LOCKS
extern bool debug_pe_locks;

#define INIT_RWLOCK(X)         {fprintf(debugf, "[%d] %s:%d:%s INITRWLOCK(%p)\n", Yap_ThreadID(),__FILE__, __LINE__, __FUNCTION__,&(X)); pthread_rwlock_init(&(X), NULL);}
#define DESTROY_RWLOCK(X)      (fprintf(debugf, "[%d] %s:%d:%s DESTROYRWLOCK(%p)\n", Yap_ThreadID(),__FILE__, __LINE__, __FUNCTION__,&(X)) && pthread_rwlock_destroy(&(X)) )
#define READ_LOCK(X) do{fprintf(debugf, "[%d] %s:%d:%s RLOCK(%p)\n", Yap_ThreadID(),__FILE__, __LINE__, __FUNCTION__,&(X)); assert(!pthread_rwlock_rdlock(&(X)));}while(0);
#define READ_UNLOCK(X) do{fprintf(debugf, "[%d] %s:%d:%s RUNLOCK(%p)\n", Yap_ThreadID(),__FILE__, __LINE__, __FUNCTION__,&(X)); assert(!pthread_rwlock_unlock(&(X)));}while(0);
#define WRITE_LOCK(X) do{fprintf(debugf, "[%d] %s:%d:%s WLOCK(%p)\n", Yap_ThreadID(),__FILE__, __LINE__, __FUNCTION__,&(X)); assert(!pthread_rwlock_wrlock(&(X)));}while(0);
#define WRITE_UNLOCK(X) do{fprintf(debugf, "[%d] %s:%d:%s WUNLOCK(%p)\n", Yap_ThreadID(),__FILE__, __LINE__, __FUNCTION__,&(X)); assert(!pthread_rwlock_unlock(&(X)));}while(0);

#else
#define INIT_RWLOCK(X)         pthread_rwlock_init(&(X), NULL)
#define DESTROY_RWLOCK(X)      pthread_rwlock_destroy(&(X))
#define READ_LOCK(X)           pthread_rwlock_rdlock(&(X))
#define READ_UNLOCK(X)         pthread_rwlock_unlock(&(X))
#define WRITE_LOCK(X)          pthread_rwlock_wrlock(&(X))
#define WRITE_UNLOCK(X)        pthread_rwlock_unlock(&(X))
#endif


#define TRUE_FUNC_WRITE_LOCK(F) WRITE_LOCK((F)->FRWLock)
#define TRUE_FUNC_WRITE_UNLOCK(F) WRITE_UNLOCK((F)->FRWLock)

#if THREADS

/* pthread mutex */

#if DEBUG_LOCKS

#define MUTEX_LOCK(LOCK_VAR)     (void)( ( debug_locks ? fprintf(stderr,"[%d] %s:%d: MULOCK(%p)\n",  Yap_ThreadID(), \
							       __FILE__, __LINE__,(LOCK_VAR)) : 1 ) && \
				   pthread_mutex_lock((LOCK_VAR)) )
#define MUTEX_TRYLOCK(LOCK_VAR)  pthread_mutex_trylock((LOCK_VAR))
#define MUTEX_UNLOCK(LOCK_VAR)  (void)((debug_locks? fprintf(stderr,"[%d] %s:%d: UNMULOCK(%p)\n",  Yap_ThreadID(), \
							     __FILE__, __LINE__,(LOCK_VAR)) : 1) && \
				  pthread_mutex_unlock((LOCK_VAR)))
#else
#define MUTEX_LOCK(LOCK_VAR) pthread_mutex_lock((LOCK_VAR))
#define MUTEX_TRYLOCK(LOCK_VAR)  pthread_mutex_trylock((LOCK_VAR))
#define MUTEX_UNLOCK(LOCK_VAR) pthread_mutex_unlock((LOCK_VAR))
#endif

#else

#define MUTEX_LOCK(LOCK_VAR) 
#define MUTEX_TRYLOCK(LOCK_VAR)
#define MUTEX_UNLOCK(LOCK_VAR) 


#endif

#endif // LOCK_PTHREAD_H
