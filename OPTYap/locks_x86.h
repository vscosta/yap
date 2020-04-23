/************************************************************************
**                                                                     **
**                   The YapTab/YapOr/OPTYap systems                   **
**                                                                     **
** YapTab extends the Yap Prolog engine to support sequential tabling  **
** YapOr extends the Yap Prolog engine to support or-parallelism       **
** OPTYap extends the Yap Prolog engine to support or-parallel tabling **
**                                                                     **
**                                                                     **
**      YAP Prolog was developed at University of Porto, Portugal      **
**                                                                     **
************************************************************************/

/************************************************************************
**                        Atomic locks for X86                         **
************************************************************************/

typedef struct {
    volatile unsigned int lock;
} spinlock_t;

static inline int
spin_trylock(spinlock_t *lock)
{
    char tmp = 1;
    __asm__ __volatile__(
			 "xchgb %b0, %1"
			 : "=q"(tmp), "=m"(lock->lock)
			 : "0"(tmp) : "memory");
    return tmp == 0;
}

static inline void
spin_unlock(spinlock_t *lock)
{
    /* To unlock we move 0 to the lock.
     * On i386 this needs to be a locked operation
     * to avoid Pentium Pro errata 66 and 92.
     */
#if defined(__x86_64__)
    __asm__ __volatile__("" : : : "memory");
    *(unsigned char*)&lock->lock = 0;
#else
    char tmp = 0;
    __asm__ __volatile__(
			 "xchgb %b0, %1"
			 : "=q"(tmp), "=m"(lock->lock)
			 : "0"(tmp) : "memory");
#endif
}


#define INIT_LOCK(LOCK_VAR)    ((LOCK_VAR) = 0)
#define TRY_LOCK(LOCK_VAR)  spin_trylock((spinlock_t *)(LOCK_VAR))

//#define DEBUG_LOCKS 1
#if DEBUG_LOCKS

extern int debug_locks;
#define LOCK(LOCK_VAR)         do {	\
    if (debug_locks) fprintf(stderr,"[%d] %s:%d: LOCK(%p)\n",	\
	    (int)pthread_self(),		     \
	    __BASE_FILE__, __LINE__,&(LOCK_VAR));    \
             if (TRY_LOCK(&(LOCK_VAR))) break;      \
		                 while (IS_LOCKED(LOCK_VAR)) continue;  \
                               } while (1)
#define IS_LOCKED(LOCK_VAR)    ((LOCK_VAR) != 0)
#define IS_UNLOCKED(LOCK_VAR)  ((LOCK_VAR) == 0)
#define UNLOCK(LOCK_VAR)       if (debug_locks) fprintf(stderr,"[%d] %s:%d: UNLOCK(%p)\n", \
				       (int)pthread_self(),		\
		      __BASE_FILE__, __LINE__,&(LOCK_VAR)); \
	     spin_unlock((spinlock_t *)&(LOCK_VAR))
#else
#define LOCK(LOCK_VAR)         { do {					\
                                 if (TRY_LOCK(&(LOCK_VAR))) break;      \
		                 while (IS_LOCKED(LOCK_VAR)) continue;  \
				 } while (1); }
#define IS_LOCKED(LOCK_VAR)    ((LOCK_VAR) != 0)
#define IS_UNLOCKED(LOCK_VAR)  ((LOCK_VAR) == 0)
#define UNLOCK(LOCK_VAR)       spin_unlock((spinlock_t *)&(LOCK_VAR))
#endif

/* the code that follows has been adapted from the Erlang sources */

typedef struct {
    volatile int lock;
} rwlock_t;

#define RWLOCK_OFFSET (1<<24)

static inline void
init_rwlock(rwlock_t *lock)
{
    lock->lock = 0;
}

static inline void
read_unlock(rwlock_t *lock)
{
    __asm__ __volatile__(
			 "lock; decl %0"
			 : "=m"(lock->lock)
			 : "m"(lock->lock)
			 );
}

static inline int
read_trylock(rwlock_t *lock)
{
    int tmp;

    tmp = 1;
    __asm__ __volatile__(
			 "lock; xaddl %0, %1"
			 : "=r"(tmp)
			 : "m"(lock->lock), "0"(tmp));
    /* tmp is now the lock's previous value */
    if (__builtin_expect(tmp >= 0, 1))
      return 1;
    read_unlock(lock);
    return 0;
}

static inline int
read_is_locked(rwlock_t *lock)
{
    return lock->lock < 0;
}

static inline void
read_lock(rwlock_t *lock)
{
    for(;;) {
      if (__builtin_expect(read_trylock(lock) != 0, 1))
	break;
      do {
	__asm__ __volatile__("rep;nop" : "=m"(lock->lock) : : "memory");
      } while (read_is_locked(lock));
    }
}

static inline void
write_unlock(rwlock_t *lock)
{
    __asm__ __volatile__(
			 "lock; addl %2,%0"
			 : "=m"(lock->lock)
			 : "m"(lock->lock), "i"(RWLOCK_OFFSET));
}

static inline int
write_trylock(rwlock_t *lock)
{
    int tmp;

    tmp = -RWLOCK_OFFSET;
    __asm__ __volatile__(
			 "lock; xaddl %0, %1"
			 : "=r"(tmp)
			 : "m"(lock->lock), "0"(tmp));
    /* tmp is now the lock's previous value */
    if (__builtin_expect(tmp == 0, 1))
      return 1;
    write_unlock(lock);
    return 0;
}

static inline int
write_is_locked(rwlock_t *lock)
{
    return lock->lock != 0;
}

static inline void
write_lock(rwlock_t *lock)
{
    for(;;) {
      if (__builtin_expect(write_trylock(lock) != 0, 1))
	break;
      do {
	__asm__ __volatile__("rep;nop" : "=m"(lock->lock) : : "memory");
      } while (write_is_locked(lock));
    }
}

#define INIT_RWLOCK(lock) init_rwlock(&(lock))
#define READ_LOCK(lock) read_lock(&(lock))
#define READ_UNLOCK(lock) read_unlock(&(lock))
#define WRITE_LOCK(lock) write_lock(&(lock))
#define WRITE_UNLOCK(lock) write_unlock(&(lock))


#if THREADS

/* pthread mutex */

#if DEBUG_LOCKS

#define MUTEX_LOCK(LOCK_VAR)     ((debug_locks ?  fprintf(stderr,"[%d] %s:%d: MULOCK(%p)\n",  (int)pthread_self(), \
							  __BASE_FILE__, __LINE__,(LOCK_VAR)) : 1), \
				                   pthread_mutex_lock((LOCK_VAR)) )
#define MUTEX_TRYLOCK(LOCK_VAR) pthread_mutex_trylock(LOCK_VAR)
#define MUTEX_UNLOCK(LOCK_VAR) if       ((debug_locks ?  fprintf(stderr,"[%d] %s:%d: MUNLOCK(%p)\n",  (int)pthread_self(), \
							  __BASE_FILE__, __LINE__,(LOCK_VAR)) : 1),     \
					 pthread_mutex_unlock((LOCK_VAR)) )
#else
#define MUTEX_LOCK(LOCK_VAR) pthread_mutex_lock(LOCK_VAR)
#define MUTEX_TRYLOCK(LOCK_VAR) pthread_mutex_trylock(LOCK_VAR)
#define MUTEX_UNLOCK(LOCK_VAR) pthread_mutex_unlock(LOCK_VAR)
#endif

#else

#define MUTEX_LOCK(LOCK_VAR) 
#define MUTEX_TRYLOCK(LOCK_VAR) 
#define MUTEX_UNLOCK(LOCK_VAR) 


#endif
