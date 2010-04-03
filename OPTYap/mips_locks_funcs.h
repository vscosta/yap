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
**                          Atomic locks for MIPS                      **
************************************************************************/

static __inline__ int test_and_set_bit(int nr, volatile void *addr)
{
	int	mask, retval, mw;

	mask = 1;
	do {
		mw = load_linked(addr);
		retval = (mask & mw) != 0;
	} while (!store_conditional(addr, mw|mask));

	return retval;
}

static inline void _spin_lock(__dummy_lock_t *lock)
{
	unsigned int tmp;

	__asm__ __volatile__(
	".set\tnoreorder\t\t\t# spin_lock\n"
	"1:\tll\t%1, %2\n\t"
	"bnez\t%1, 1b\n\t"
	" li\t%1, 1\n\t"
	"sc\t%1, %0\n\t"
	"beqz\t%1, 1b\n\t"
	" sync\n\t"
	".set\treorder"
	: "=o" (__dummy_lock(lock)), "=&r" (tmp)
	: "o" (__dummy_lock(lock))
	: "memory");
}

static inline void spin_unlock(__dummy_lock_t *lock)
{
	__asm__ __volatile__(
	".set\tnoreorder\t\t\t# spin_unlock\n\t"
	"sync\n\t"
	"sw\t$0, %0\n\t"
	".set\treorder"	
	: "=o" (__dummy_lock(lock))
	: "o" (__dummy_lock(lock))
	: "memory");
}

static inline void _read_lock(rwlock_t *rw)
{
	unsigned int tmp;

	__asm__ __volatile__(
	".set\tnoreorder\t\t\t# read_lock\n"
	"1:\tll\t%1, %2\n\t"
	"bltz\t%1, 1b\n\t"
	" addu\t%1, 1\n\t"
	"sc\t%1, %0\n\t"
	"beqz\t%1, 1b\n\t"
	" sync\n\t"
	".set\treorder"	
	: "=o" (__dummy_lock(rw)), "=&r" (tmp)
	: "o" (__dummy_lock(rw))
	: "memory");
}

/* Note the use of sub, not subu which will make the kernel die with an
   overflow exception if we ever try to unlock an rwlock that is already
   unlocked or is being held by a writer.  */
static inline void _read_unlock(rwlock_t *rw)
{
	unsigned int tmp;

	__asm__ __volatile__(
	".set\tnoreorder\t\t\t# read_unlock\n"
	"1:\tll\t%1, %2\n\t"
	"sub\t%1, 1\n\t"
	"sc\t%1, %0\n\t"
	"beqz\t%1, 1b\n\t"
	".set\treorder"	
	: "=o" (__dummy_lock(rw)), "=&r" (tmp)
	: "o" (__dummy_lock(rw))
	: "memory");
}

static inline void _write_lock(rwlock_t *rw)
{
	unsigned int tmp;

	__asm__ __volatile__(
	".set\tnoreorder\t\t\t# write_lock\n"
	"1:\tll\t%1, %2\n\t"
	"bnez\t%1, 1b\n\t"
	" lui\t%1, 0x8000\n\t"
	"sc\t%1, %0\n\t"
	"beqz\t%1, 1b\n\t"
	" sync\n\t"
	".set\treorder"	
	: "=o" (__dummy_lock(rw)), "=&r" (tmp)
	: "o" (__dummy_lock(rw))
	: "memory");
}

static inline void _write_unlock(rwlock_t *rw)
{
	__asm__ __volatile__(
	".set\tnoreorder\t\t\t# write_unlock\n\t"
	"sync\n\t"
	"sw\t$0, %0\n\t"
	".set\treorder"	
	: "=o" (__dummy_lock(rw))
	: "o" (__dummy_lock(rw))
	: "memory");
}


