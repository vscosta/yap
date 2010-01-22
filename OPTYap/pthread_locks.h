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
* File:		pthread_locks.h                                          *
* Last rev:								 *
* mods:									 *
* comments:	                                                         *
*									 *
*************************************************************************/

#include <pthread.h>

/* ----------------------------------- **
**      Atomic locks for PTHREADS      **
** ----------------------------------- */

#define INIT_LOCK(LOCK_VAR)    pthread_mutex_init(&(LOCK_VAR), NULL)
#define DESTROY_LOCK(LOCK_VAR) pthread_mutex_destroy(&(LOCK_VAR))
#define TRY_LOCK(LOCK_PTR)     pthread_mutex_trylock(&(LOCK_VAR))
#define LOCK(LOCK_VAR)         pthread_mutex_lock(&(LOCK_VAR))
#define UNLOCK(LOCK_VAR)       pthread_mutex_unlock(&(LOCK_VAR))
static inline int
xIS_LOCKED(pthread_mutex_t *LOCK_VAR) {
  if (pthread_mutex_trylock(LOCK_VAR) == 0) {
    pthread_mutex_unlock(LOCK_VAR);
    return TRUE;
  }
  return FALSE;
}
static inline int
xIS_UNLOCKED(pthread_mutex_t *LOCK_VAR) {
  if (pthread_mutex_trylock(LOCK_VAR) == 0) {
    pthread_mutex_unlock(LOCK_VAR);
    return FALSE;
  }
  return TRUE;
}
#define IS_LOCKED(LOCK_VAR)    xIS_LOCKED(&(LOCK_VAR))
#define IS_UNLOCKED(LOCK_VAR)  xIS_UNLOCKED(&(LOCK_VAR))

#define INIT_RWLOCK(X)         pthread_rwlock_init(&(X), NULL)
#define DESTROY_RWLOCK(X)      pthread_rwlock_destroy(&(X))
#define READ_LOCK(X)           pthread_rwlock_rdlock(&(X))
#define READ_UNLOCK(X)         pthread_rwlock_unlock(&(X))
#define WRITE_LOCK(X)          pthread_rwlock_wrlock(&(X))
#define WRITE_UNLOCK(X)        pthread_rwlock_unlock(&(X))
