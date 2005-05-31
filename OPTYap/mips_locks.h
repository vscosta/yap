/**********************************************************************
                                                               
                       The OPTYap Prolog system                
  OPTYap extends the Yap Prolog system to support or-parallel tabling
                                                               
  Copyright:   R. Rocha and NCC - University of Porto, Portugal
  File:        mips_locks.h  
  version:     $Id: mips_locks.h,v 1.2 2005-05-31 08:24:24 ricroc Exp $   
                                                                     
**********************************************************************/

/* ------------------------------- **
**      Atomic locking for MIPS    **
** ------------------------------- */

/* This code is stolen from the Linux kernel */

typedef struct { unsigned long a[100]; } __dummy_lock_t;
#define __dummy_lock(lock) (*(__dummy_lock_t *)(lock))

#define load_linked(addr)                                       \
({                                                              \
	unsigned int __res;                                     \
                                                                \
	__asm__ __volatile__(                                   \
	"ll\t%0,(%1)"                                           \
	: "=r" (__res)                                          \
	: "r" ((unsigned long) (addr)));                        \
                                                                \
	__res;                                                  \
})

#define store_conditional(addr,value)                           \
({                                                              \
	int	__res;                                          \
                                                                \
	__asm__ __volatile__(                                   \
	"sc\t%0,(%2)"                                           \
	: "=r" (__res)                                          \
	: "0" (value), "r" (addr));                             \
                                                                \
	__res;                                                  \
})

#define INIT_LOCK(LOCK_VAR)    ((LOCK_VAR) = 0)
#define TRY_LOCK(LOCK_PTR)  (!test_and_set_bit(0,(__dumy_lock_t *)(LOCK_PTR)))
#define LOCK(LOCK_VAR)      _spin_lock((__dummy_lock_t *)(&(LOCK_VAR)))
#define IS_LOCKED(LOCK_VAR)    ((LOCK_VAR) != 0)
#define IS_UNLOCKED(LOCK_VAR)  ((LOCK_VAR) == 0)


/* We need to support sync on MIPS */
#define UNLOCK(LOCK_VAR)     spin_unlock((__dummy_lock_t *)(&(LOCK_VAR)))

typedef struct {
	volatile unsigned int lock;
} rwlock_t;

#define RW_LOCK_UNLOCKED (rwlock_t) { 0 }

#define READ_LOCK(X) _read_lock(&(X))

#define READ_UNLOCK(X) _read_unlock(&(X))

#define WRITE_LOCK(X)  _write_lock(&(X))

#define WRITE_UNLOCK(X)  _write_unlock(&(X))

#define INIT_RWLOCK(RW)   (RW) = RW_LOCK_UNLOCKED
