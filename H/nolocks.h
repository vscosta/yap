/* ----------------------------- **
**     no locks                  **
** ----------------------------- */

#define INIT_LOCK(LOCK_VAR)
#define LOCK(LOCK_VAR)
#define UNLOCK(LOCK_VAR)
#define IS_LOCKED(LOCK_VAR)
#define IS_UNLOCKED(LOCK_VAR)

#define READ_LOCK(RWLOCK_VAR)
#define READ_UNLOCK(RWLOCK_VAR)
#define WRITE_LOCK(RWLOCK_VAR)        YAPEnterCriticalSection()
#define WRITE_UNLOCK(RWLOCK_VAR)      YAPLeaveCriticalSection()
#define INIT_RWLOCK(RWLOCK_VAR)   


#define MUTEX_LOCK(LOCK_VAR)
#define MUTEX_TRYLOCK(LOCK_VAR)
#define MUTEX_UNLOCK(LOCK_VAR)
