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

#ifndef LOCK_PTHREAD_H0

#define LOCK_PTHREAD_H 1

#include <pthread.h>

//#define DEBUG_PE_LOCKS 1
//#define DEBUG_LOCKS 1
#include <stdio.h>

int Yap_ThreadID( void );
#define debugf ( stderr ? stderr : stdout )

#define INIT_LOCK(LOCK_VAR)    pthread_mutex_init(&(LOCK_VAR), NULL)
#define DESTROY_LOCK(LOCK_VAR) pthread_mutex_destroy(&(LOCK_VAR))
#define TRY_LOCK(LOCK_VAR)     pthread_mutex_trylock(&(LOCK_VAR))
#if DEBUG_LOCKS
extern bool debug_locks;

#define LOCK(LOCK_VAR)         (void)(fprintf(debugf, "[%d] %s:%d: LOCK(%p)\n",  Yap_ThreadID(),__BASE_FILE__, __LINE__,&(LOCK_VAR))  && pthread_mutex_lock(&(LOCK_VAR)) )
#define UNLOCK(LOCK_VAR)       (void)(fprintf(debugf, "[%d] %s:%d: UNLOCK(%p)\n",  Yap_ThreadID(),__BASE_FILE__, __LINE__,&(LOCK_VAR))  && pthread_mutex_unlock(&(LOCK_VAR)) )
#else
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


#define INIT_RWLOCK(X)         pthread_rwlock_init(&(X), NULL)
#define DESTROY_RWLOCK(X)      pthread_rwlock_destroy(&(X))
#if DEBUG_PE_LOCKS
extern bool debug_pe_locks;

#define READ_LOCK(X) ((debug_pe_locks ?					\
		      fprintf(debugf, "[%d] %s:%d: RLOCK(%p)\n",  \
			      Yap_ThreadID(),__BASE_FILE__, __LINE__,&(X)) \
		       : 1) && pthread_rwlock_rdlock(&(X)) )
#define READ_UNLOCK(X) ((debug_pe_locks ?					\
		      fprintf(debugf, "[%d] %s:%d: UNLOCK(%p)\n", \
			      Yap_ThreadID(),__BASE_FILE__, __LINE__,&(X)) \
			 : 1) && pthread_rwlock_unlock(&(X)) )
#define WRITE_LOCK(X) ((debug_pe_locks ?					\
		      fprintf(debugf, "[%d] %s:%d: RLOCK(%p)\n", \
			      Yap_ThreadID(),__BASE_FILE__, __LINE__,&(X)) \
			: 1) && pthread_rwlock_rdlock(&(X)) )
#define WRITE_UNLOCK(X) ((debug_pe_locks ?					\
		      fprintf(debugf, "[%d] %s:%d: UNLOCK(%p)\n",  \
			      Yap_ThreadID(),__BASE_FILE__, __LINE__,&(X))\
			  : 1) && pthread_rwlock_unlock(&(X)) )

#else
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
							       __BASE_FILE__, __LINE__,(LOCK_VAR)) : 1 ) && \
				   pthread_mutex_lock((LOCK_VAR)) )
#define MUTEX_TRYLOCK(LOCK_VAR)  pthread_mutex_trylock((LOCK_VAR))
#define MUTEX_UNLOCK(LOCK_VAR)  (void)((debug_locks? fprintf(stderr,"[%d] %s:%d: UNMULOCK(%p)\n",  Yap_ThreadID(), \
							     __BASE_FILE__, __LINE__,(LOCK_VAR)) : 1) && \
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
