/**********************************************************************
                                                               
                       The OPTYap Prolog system                
  OPTYap extends the Yap Prolog system to support or-parallel tabling
                                                               
  Copyright:   R. Rocha and NCC - University of Porto, Portugal
  File:        x86_locks.h
  version:     $Id: x86_locks.h,v 1.4 2007-11-26 23:43:09 vsc Exp $   
                                                                     
**********************************************************************/

/* ----------------------------- **
**      Atomic lock for X86      **
** ----------------------------- */

#define swap(reg,adr)					     \
({							     \
   char _ret;						     \
   asm volatile ("xchgb %0,%1"				     \
	: "=q" (_ret), "=m" (*(adr))	/* Output %0,%1 */   \
	: "m"  (*(adr)), "0"  (reg));	/* Input (%2),%0 */  \
   _ret;						     \
})

#define TRY_LOCK(LOCK_VAR)  (swap(1,(LOCK_VAR))==0)

#define INIT_LOCK(LOCK_VAR)    ((LOCK_VAR) = 0)
#define LOCK(LOCK_VAR)         do {	\
                                 if (TRY_LOCK(&(LOCK_VAR))) break;      \
		                 while (IS_LOCKED(LOCK_VAR)) continue;  \
                               } while (1)
#define IS_LOCKED(LOCK_VAR)    ((LOCK_VAR) != 0)
#define IS_UNLOCKED(LOCK_VAR)  ((LOCK_VAR) == 0)
#define UNLOCK(LOCK_VAR)       ((LOCK_VAR) = 0)

/* This code has been copied from the sources of the Linux kernel */

/*
 * On x86, we implement read-write locks as a 32-bit counter
 * with the high bit (sign) being the "contended" bit.
 *
 * The inline assembly is non-obvious. Think about it.
 *
 * Changed to use the same technique as rw semaphores.  See
 * semaphore.h for details.  -ben
 */
/* the spinlock helpers are in arch/i386/kernel/semaphore.S */

typedef struct { unsigned long a[100]; } __dummy_lock_t;
#define __dummy_lock(lock) (*(__dummy_lock_t *)(lock))

typedef struct { volatile unsigned int lock; } rwlock_t;

#define RW_LOCK_BIAS		 0x01000000
#define RW_LOCK_BIAS_STR	"0x01000000"

#define RW_LOCK_UNLOCKED          RW_LOCK_BIAS

#define __build_read_lock(rw, helper)   \
	asm volatile("lock\n" \
                     "subl $1,(%0)\n\t" \
		     "js 2f\n" \
		     "1:\n" \
		     ".section .text.lock,\"ax\"\n" \
		     "2:\tcall __read_lock_failed\n\t" \
		     "jmp 1b\n" \
		     ".previous" \
		     ::"a" (rw) : "memory")

#define __build_write_lock(rw, helper) \
	asm volatile("lock\n"\
                     "subl $" RW_LOCK_BIAS_STR ",(%0)\n\t" \
		     "jnz 2f\n" \
		     "1:\n" \
		     ".section .text.lock,\"ax\"\n" \
		     "2:\tcall __write_lock_failed\n\t" \
		     "jmp 1b\n" \
		     ".previous" \
		     ::"a" (rw) : "memory")

static inline void read_lock(rwlock_t *rw)
{
	__build_read_lock(rw, "__read_lock_failed");
}

static inline void write_lock(rwlock_t *rw)
{
	__build_write_lock(rw, "__write_lock_failed");
}


#define READ_LOCK(X)  read_lock(&(X))
#define WRITE_LOCK(X)  write_lock(&(X))

#define READ_UNLOCK(rw)		asm volatile("lock ; incl %0" :"=m" (__dummy_lock(&(rw))))
#define WRITE_UNLOCK(rw)	asm volatile("lock ; addl $" RW_LOCK_BIAS_STR ",%0":"=m" (__dummy_lock(&(rw))))

#define INIT_RWLOCK(RW)   (RW).lock = RW_LOCK_UNLOCKED


