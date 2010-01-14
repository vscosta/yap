/**********************************************************************
                                                               
                       The OPTYap Prolog system                
  OPTYap extends the Yap Prolog system to support or-parallel tabling
                                                               
  Copyright:   R. Rocha and NCC - University of Porto, Portugal
  File:        opt.misc.c  
  version:     $Id: opt.misc.c,v 1.11 2005-06-03 18:28:11 ricroc Exp $   
                                                                     
**********************************************************************/

/* ------------------ **
**      Includes      **
** ------------------ */

#include "Yap.h"
#if defined(YAPOR) || defined(TABLING)
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_STDARG_H
#include <stdarg.h>
#endif /* HAVE_STDARG_H */
#include "Yatom.h"
#include "yapio.h"



/* ------------------------------------------- **
**      Global variables are defined here      **
** ------------------------------------------- */

#ifndef THREADS
struct local_data *LOCAL;
#ifdef YAPOR
struct worker WORKER;
#endif /* YAPOR */
#endif


/* -------------------------- **
**      Global functions      **
** -------------------------- */

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


#if defined(YAPOR_ERRORS) || defined(TABLING_ERRORS)
void error_message(const char *mesg, ...) {
  va_list args;
  va_start(args, mesg);
#ifdef YAPOR
  LOCK(GLOBAL_LOCKS_stderr_messages);
#endif /* YAPOR */
  fprintf(stderr, "% POTENCIAL ERROR- ");
#ifdef YAPOR
  fprintf(stderr, "W%d: ", worker_id);
#endif /* YAPOR */
  vfprintf(stderr, mesg, args);
  fprintf(stderr, "\n");
#ifdef YAPOR
  UNLOCK(GLOBAL_LOCKS_stderr_messages);
#endif /* YAPOR */
  return;
}
#endif /* YAPOR_ERRORS || TABLING_ERRORS */
#endif /* YAPOR || TABLING */
