/* ------------------ **
**      Includes      **
** ------------------ */

#include "Yap.h"
#if defined(YAPOR) || defined(TABLING)
#include <stdarg.h>
#include <stdio.h>
#include "Yatom.h"
#include "yapio.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STDARG_H
#include <stdarg.h>
#endif



/* ------------------------------------------- **
**      Global variables are defined here      **
** ------------------------------------------- */

struct global_data *GLOBAL;
struct local_data *LOCAL;
#ifdef YAPOR
struct local_data *REMOTE[MAX_WORKERS];
struct worker WORKER;
#endif /* YAPOR */



/* -------------------------- **
**      Global functions      **
** -------------------------- */

void abort_optyap(const char *msg, ...) {
  va_list args;

  va_start(args, msg);
  fprintf(stderr, "[ ");
#ifdef YAPOR
  fprintf (stderr, "Worker %d ", worker_id);
#endif /* YAPOR */
  fprintf (stderr, "Aborting OPTYap -> ");
  vfprintf(stderr, msg, args);
  fprintf(stderr, " ]\n");

#ifdef YAPOR
  unmap_memory();
#endif
  exit (1);
}


void itos(int i, char *s) {
  int n,r,j;
  n = 10;
  while (n <= i) n *= 10;
  j = 0;
  while (n > 1) {
    n = n / 10;   
    r = i / n;
    i = i - r * n;
    s[j++] = r + '0';
  }
  s[j] = 0;
  return;
}


void information_message(const char *mesg,...) {
  va_list args;
  va_start(args, mesg);
  fprintf(stderr, "[ ");
  vfprintf(stderr, mesg, args);
  fprintf(stderr, " ]\n");
  return;
}
/* ------------------------- **
**      Local functions      **
** ------------------------- */

int tabling_putchar(int sno, int ch) {
  return(YP_putc(ch, stderr));
}
#endif /* TABLING_DEBUG */

#ifdef YAPOR

#if defined(YAPOR_ERRORS) || defined(TABLING_ERRORS)
void error_message(const char *mesg, ...) {
  va_list args;
  va_start(args, mesg);
#ifdef YAPOR
  LOCK(GLOBAL_LOCKS_stderr_messages);
#endif /* YAPOR */
  fprintf(stderr, "[ ");
#ifdef YAPOR
  fprintf(stderr, "W%d: ", worker_id);
#endif /* YAPOR */
  fprintf(stderr, "Potencial Error -> ");
  vfprintf(stderr, mesg, args);
  fprintf(stderr, " ]\n");
#ifdef YAPOR
  UNLOCK(GLOBAL_LOCKS_stderr_messages);
#endif /* YAPOR */
  return;
}
#endif /* YAPOR_ERRORS || TABLING_ERRORS */


#ifdef YAPOR
#ifdef sparc
void rw_lock_voodoo(void) {
  /* code taken from the Linux kernel, it handles shifting between locks */
  /* Read/writer locks, as usual this is overly clever to make it as fast as possible. */
	/* caches... */
	__asm__ __volatile__(
"___rw_read_enter_spin_on_wlock:"
"	orcc	%g2, 0x0, %g0"
"	be,a	___rw_read_enter"
"	 ldstub	[%g1 + 3], %g2"
"	b	___rw_read_enter_spin_on_wlock"
"	 ldub	[%g1 + 3], %g2"
"___rw_read_exit_spin_on_wlock:"
"	orcc	%g2, 0x0, %g0"
"	be,a	___rw_read_exit"
"	 ldstub	[%g1 + 3], %g2"
"	b	___rw_read_exit_spin_on_wlock"
"	 ldub	[%g1 + 3], %g2"
"___rw_write_enter_spin_on_wlock:"
"	orcc	%g2, 0x0, %g0"
"	be,a	___rw_write_enter"
"	 ldstub	[%g1 + 3], %g2"
"	b	___rw_write_enter_spin_on_wlock"
"	 ld	[%g1], %g2"
""
"	.globl	___rw_read_enter"
"___rw_read_enter:"
"	orcc	%g2, 0x0, %g0"
"	bne,a	___rw_read_enter_spin_on_wlock"
"	 ldub	[%g1 + 3], %g2"
"	ld	[%g1], %g2"
"	add	%g2, 1, %g2"
"	st	%g2, [%g1]"
"	retl"
"	 mov	%g4, %o7"

"	.globl	___rw_read_exit"
"___rw_read_exit:"
"	orcc	%g2, 0x0, %g0"
"	bne,a	___rw_read_exit_spin_on_wlock"
"	 ldub	[%g1 + 3], %g2"
"	ld	[%g1], %g2"
"	sub	%g2, 0x1ff, %g2"
"	st	%g2, [%g1]"
"	retl"
"	 mov	%g4, %o7"

"	.globl	___rw_write_enter"
"___rw_write_enter:"
"	orcc	%g2, 0x0, %g0"
"	bne	___rw_write_enter_spin_on_wlock"
"	 ld	[%g1], %g2"
"	andncc	%g2, 0xff, %g0"
"	bne,a	___rw_write_enter_spin_on_wlock"
"	 stb	%g0, [%g1 + 3]"
"	retl"
"	 mov	%g4, %o7"
   );
}
#endif /* sparc */


#ifdef i386
asm(

".align	4"
".globl	__write_lock_failed"
"__write_lock_failed:"
"	lock;   addl	$" RW_LOCK_BIAS_STR ",(%eax)"
"1:	cmpl	$" RW_LOCK_BIAS_STR ",(%eax)"
"	jne	1b"
""
"	lock;   subl	$" RW_LOCK_BIAS_STR ",(%eax)"
"	jnz	__write_lock_failed"
"	ret"
""
""
".align	4"
".globl	__read_lock_failed"
"__read_lock_failed:"
"	lock ; incl	(%eax)"
"1:	cmpl	$1,(%eax)"
"	js	1b"
""
"	lock ; decl	(%eax)"
"	js	__read_lock_failed"
"	ret"

);
#endif /* i386 */
#endif /* YAPOR */
#endif /* YAPOR || TABLING */
