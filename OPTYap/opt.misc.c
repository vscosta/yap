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
  return(putc(ch, stderr));
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

#endif /* YAPOR || TABLING */
