/**********************************************************************
                                                               
                       The OPTYap Prolog system                
  OPTYap extends the Yap Prolog system to support or-parallel tabling
                                                               
  Copyright:   R. Rocha and NCC - University of Porto, Portugal
  File:        alpha_locks.h  
  version:     $Id: alpha_locks.h,v 1.2 2005-05-31 08:24:24 ricroc Exp $   
                                                                     
**********************************************************************/

/* ------------------------------- **
**      Atomic lock for ALPHA      **
** ------------------------------- */

/* This code is stolen from the Linux kernel */

typedef struct { unsigned long a[100]; } __dummy_lock_t;
#define __dummy_lock(lock) (*(__dummy_lock_t *)(lock))

/*
  Memory barrier in Alpha machines
*/
#define mb() \
__asm__ __volatile__("mb": : :"memory")

#define INIT_LOCK(LOCK_VAR)    ((LOCK_VAR) = 0)
#define TRY_LOCK(LOCK_PTR)  (!_test_and_set_bit(0,(volatile void *)(LOCK_PTR)))
#define LOCK(LOCK_VAR)        _spin_lock((volatile void *)&(LOCK_VAR))
#define IS_LOCKED(LOCK_VAR)    ((LOCK_VAR) != 0)
#define IS_UNLOCKED(LOCK_VAR)  ((LOCK_VAR) == 0)

/* We need to support a memory barrier on Alphas */
#define UNLOCK(LOCK_VAR)     { mb(); LOCK_VAR = 0; }

typedef struct {
	volatile int write_lock:1, read_counter:31;
} /*__attribute__((aligned(32)))*/ rwlock_t;

#define RW_LOCK_UNLOCKED (rwlock_t) { 0, 0 }

#define READ_LOCK(X) _read_lock(&(X))

#define READ_UNLOCK(X) _read_unlock(&(X))

#define WRITE_LOCK(X)  _write_lock(&(X))

#define WRITE_UNLOCK(X)  _write_unlock(&(X))

#define INIT_RWLOCK(RW)   (RW) = RW_LOCK_UNLOCKED
