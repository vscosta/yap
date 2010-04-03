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
**                        Atomic locks for ALPHA                       **
************************************************************************/

/* This code is stolen from the Linux kernel */

static __inline__ int _test_and_set_bit(unsigned long nr,
				       volatile void * addr)
{
	unsigned long oldbit;
	unsigned long temp;
	unsigned int * m = ((unsigned int *) addr) + (nr >> 5);

	__asm__ __volatile__(
	"1:	ldl_l %0,%1\n"
	"	and %0,%3,%2\n"
	"	bne %2,2f\n"
	"	xor %0,%3,%0\n"
	"	stl_c %0,%1\n"
	"	beq %0,3f\n"
	"	mb\n"
	"2:\n"
	".subsection 2\n"
	"3:	br 1b\n"
	".previous"
	:"=&r" (temp), "=m" (*m), "=&r" (oldbit)
	:"Ir" (1UL << (nr & 31)), "m" (*m));

	return oldbit != 0;
}

static __inline__ void _spin_lock(volatile void *lock)
{
	long tmp;

	/* Use sub-sections to put the actual loop at the end
	   of this object file's text section so as to perfect
	   branch prediction.  */
	__asm__ __volatile__(
	"1:	ldl_l	%0,%1\n"
	"	blbs	%0,2f\n"
	"	or	%0,1,%0\n"
	"	stl_c	%0,%1\n"
	"	beq	%0,2f\n"
	"	mb\n"
	".subsection 2\n"
	"2:	ldl	%0,%1\n"
	"	blbs	%0,2b\n"
	"	br	1b\n"
	".previous"
	: "=r" (tmp), "=m" (__dummy_lock(lock))
	: "m"(__dummy_lock(lock)));
}

static inline void _write_lock(rwlock_t * lock)
{
	long regx;

	__asm__ __volatile__(
	"1:	ldl_l	%1,%0\n"
	"	bne	%1,6f\n"
	"	or	$31,1,%1\n"
	"	stl_c	%1,%0\n"
	"	beq	%1,6f\n"
	"	mb\n"
	".subsection 2\n"
	"6:	ldl	%1,%0\n"
	"	bne	%1,6b\n"
	"	br	1b\n"
	".previous"
	: "=m" (__dummy_lock(lock)), "=&r" (regx)
	: "0" (__dummy_lock(lock))
	);
}

static inline void _read_lock(rwlock_t * lock)
{
	long regx;

	__asm__ __volatile__(
	"1:	ldl_l	%1,%0\n"
	"	blbs	%1,6f\n"
	"	subl	%1,2,%1\n"
	"	stl_c	%1,%0\n"
	"	beq	%1,6f\n"
	"4:	mb\n"
	".subsection 2\n"
	"6:	ldl	%1,%0\n"
	"	blbs	%1,6b\n"
	"	br	1b\n"
	".previous"
	: "=m" (__dummy_lock(lock)), "=&r" (regx)
	: "m" (__dummy_lock(lock))
	);
}


static inline void _write_unlock(rwlock_t * lock)
{
	mb();
	*(volatile int *)lock = 0;
}

static inline void _read_unlock(rwlock_t * lock)
{
	long regx;
	__asm__ __volatile__(
	"1:	ldl_l	%1,%0\n"
	"	addl	%1,2,%1\n"
	"	stl_c	%1,%0\n"
	"	beq	%1,6f\n"
	".subsection 2\n"
	"6:	br	1b\n"
	".previous"
	: "=m" (__dummy_lock(lock)), "=&r" (regx)
	: "m" (__dummy_lock(lock)));
}

