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

#ifdef _YAP_NOT_INSTALLED_
#include "Regs.h"
#else
#include "Regs.h"
#endif

#endif
