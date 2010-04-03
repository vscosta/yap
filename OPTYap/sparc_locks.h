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

/************************************************************************
**                       Atomic locks for SPARC                        **
************************************************************************/

#define swap_il(adr,reg)				     \
({ int _ret;						     \
   asm volatile ("swap %1,%0"				     \
	: "=r" (_ret), "=m" (*(adr))	/* Output %0,%1 */   \
	: "m"  (*(adr)), "0" (reg));	/* Input (%2),%0 */  \
   _ret;						     \
})

#define TRY_LOCK(LOCK_VAR)  (swap_il((LOCK_VAR),1)==0)

#define INIT_LOCK(LOCK_VAR)    ((LOCK_VAR) = 0)
#define LOCK(LOCK_VAR)         do {                                     \
                                 if (TRY_LOCK(&(LOCK_VAR))) break;      \
		                 while (IS_LOCKED(LOCK_VAR)) continue;  \
		               } while (1)
#define IS_LOCKED(LOCK_VAR)    ((LOCK_VAR) != 0)
#define IS_UNLOCKED(LOCK_VAR)  ((LOCK_VAR) == 0)
#define UNLOCK(LOCK_VAR)       ((LOCK_VAR) = 0)

/* Read-write spinlocks, allowing multiple readers
 * but only one writer.
 *
 */

typedef struct { volatile unsigned int lock; } rwlock_t;

/* Sort of like atomic_t's on Sparc, but even more clever.
 *
 *	------------------------------------
 *	| 24-bit counter           | wlock |  rwlock_t
 *	------------------------------------
 *	 31                       8 7     0
 *
 * wlock signifies the one writer is in or somebody is updating
 * counter. For a writer, if he successfully acquires the wlock,
 * but counter is non-zero, he has to release the lock and wait,
 * till both counter and wlock are zero.
 *
 * Unfortunately this scheme limits us to ~16,000,000 cpus.
 */

static __inline__ void _read_lock(rwlock_t *rw)
{
	register rwlock_t *lp asm("g1");
	lp = rw;
	asm __volatile__("mov	%%o7, %%g4\n\t" \
	"call	___rw_read_enter\n\t" \
	"ldstub	[%%g1 + 3], %%g2\n" \
	: /* no outputs */ \
	: "r" (lp) \
	: "g2", "g4", "g7", "memory", "cc");
}


#define READ_LOCK(lock) \
do {	_read_lock(&(lock)); \
} while(0)

static __inline__ void _read_unlock(rwlock_t *rw)
{
	register rwlock_t *lp asm("g1");
	lp = rw;
	asm __volatile__( \
	"mov	%%o7, %%g4\n\t" \
	"call	___rw_read_exit\n\t" \
        "ldstub	[%%g1 + 3], %%g2\n" \
	: /* no outputs */ \
	: "r" (lp) \
	: "g2", "g4", "g7", "memory", "cc");
}

#define READ_UNLOCK(lock) \
do {	_read_unlock(&lock); \
} while(0)

static __inline__ void write_lock(rwlock_t *rw)
{
	register rwlock_t *lp asm("g1");
	lp = rw;
	asm __volatile__( \
	"mov	%%o7, %%g4\n\t"\
	"call	___rw_write_enter\n\t" \
	"ldstub	[%%g1 + 3], %%g2\n\t" \
	: /* no outputs */ \
	: "r" (lp) \
	: "g2", "g4", "g7", "memory", "cc");
}

#define WRITE_LOCK(X)  write_lock(&(X))

#define WRITE_UNLOCK(rw)	do { (&(rw))->lock = 0; } while(0)

#define INIT_RWLOCK(RW)   (RW).lock = 0
