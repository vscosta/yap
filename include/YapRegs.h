#ifndef YAP_REGS_H

#define YAP_REGS_H 1

#ifdef THREADS
#if USE_PTHREAD_LOCKING
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600
#endif  /* !_XOPEN_SOURCE */
#endif /* USE_PTHREAD_LOCKING */
#include <pthread.h>
#endif

#if defined(TABLING) || defined(YAPOR_SBA)
            typedef struct trail_frame {
  Term term;
  CELL value;
} *tr_fr_ptr;

#define TrailTerm(X)   ((X)->term)
#else
typedef Term *tr_fr_ptr;

#define TrailTerm(X)   (*(CELL*)(X))
#endif

typedef void *choiceptr;

typedef void *yamop;

typedef char *ADDR;

// #define RESET_VARIABLE(X) (*(X) = (CELL)(X))

#ifdef _YAP_NOT_INSTALLED_
#include "Regs.h"
#else
#include "src/Regs.h"
#endif

#endif
