/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
			      VU University Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef PL_THREAD_H_DEFINED
#define PL_THREAD_H_DEFINED

#ifdef THREADS
#include <pthread.h>

typedef pthread_mutex_t simpleMutex;

#define simpleMutexInit(p)	pthread_mutex_init(p, NULL)
#define simpleMutexDelete(p)	pthread_mutex_destroy(p)
#define simpleMutexLock(p)	pthread_mutex_lock(p)
#define simpleMutexUnlock(p)	pthread_mutex_unlock(p)

typedef pthread_mutex_t recursiveMutex;

#define NEED_RECURSIVE_MUTEX_INIT 1
extern int recursiveMutexInit(recursiveMutex *m);
#define recursiveMutexDelete(p)  pthread_mutex_destroy(p)
#define recursiveMutexLock(p)    pthread_mutex_lock(p)
#define recursiveMutexTryLock(p) pthread_mutex_trylock(p)
#define recursiveMutexUnlock(p)  pthread_mutex_unlock(p)


typedef struct counting_mutex
{ simpleMutex mutex;                    /* mutex itself */
  const char *name;                     /* name of the mutex */
  long count;                           /* # times locked */
  long unlocked;                        /* # times unlocked */
#ifdef O_CONTENTION_STATISTICS
  long collisions;                      /* # contentions */
#endif
  struct counting_mutex *next;          /* next of allocated chain */
} counting_mutex;

extern counting_mutex  *allocSimpleMutex(const char *name);
extern void             freeSimpleMutex(counting_mutex *m);

extern counting_mutex _PL_mutexes[];	/* Prolog mutexes */

#define L_MISC          0
#define L_ALLOC         1
#define L_ATOM          2
#define L_FLAG          3
#define L_FUNCTOR       4
#define L_RECORD        5
#define L_THREAD        6
#define L_PREDICATE     7
#define L_MODULE        8
#define L_TABLE         9
#define L_BREAK        10
#define L_FILE         11
#define L_SEETELL      12
#define L_PLFLAG       13
#define L_OP           14
#define L_INIT         15
#define L_TERM         16
#define L_GC           17
#define L_AGC          18
#define L_STOPTHEWORLD 19
#define L_FOREIGN      20
#define L_OS           21
#define L_LOCALE       23
#ifdef __WINDOWS__
#define L_DDE	       24
#define L_CSTACK       25
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The IF_MT(id, g) macro  is  used  to   bypass  mutexes  if  threading  is
disabled. We cannot do this for the L_THREAD mutex however as we need to
control when threads can be created.

We  assume  id  ==  L_THREAD  is  optimized  away  if  id  is  known  at
compile-time
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
#define IF_MT(id,g) g
//#define IF_MT(id, g) if ( id == L_THREAD  ) g

#ifdef O_CONTENTION_STATISTICS
#define countingMutexLock(cm) \
	do \
	{ if ( pthread_mutex_trylock(&(cm)->mutex) == EBUSY ) \
	  { (cm)->collisions++; \
	    pthread_mutex_lock(&(cm)->mutex); \
	  } \
	  (cm)->count++; \
	} while(0)
#else
#define countingMutexLock(cm) \
	do \
	{ simpleMutexLock(&(cm)->mutex); \
	  (cm)->count++; \
	} while(0)
#endif
#define countingMutexUnlock(cm) \
	do \
	{ (cm)->unlocked++; \
	  assert((cm)->unlocked <= (cm)->count); \
	  simpleMutexUnlock(&(cm)->mutex); \
	} while(0)

//#define O_DEBUG_MT
#ifdef O_DEBUG_MT
#define PL_LOCK(id) \
	do { Sdprintf("[%d] %s:%d: LOCK(%s)\n", \
		      pthread_self(),		     \
		      __BASE_FILE__, __LINE__, #id); \
             countingMutexLock(&_PL_mutexes[id]); \
	   } while(0)
#define PL_UNLOCK(id) \
	do { Sdprintf("[%d] %s:%d: UNLOCK(%s)\n", \
		      pthread_self(), \
		      __BASE_FILE__, __LINE__, #id); \
	     countingMutexUnlock(&_PL_mutexes[id]); \
	   } while(0)
#else
#define PL_LOCK(id)   IF_MT(id, countingMutexLock(&_PL_mutexes[id]))
#define PL_UNLOCK(id) IF_MT(id, countingMutexUnlock(&_PL_mutexes[id]))
#endif
#undef O_DEBUG_MT

#define IOLOCK  recursiveMutex

#else
#define PL_LOCK(X)
#define PL_UNLOCK(X)

typedef void *		IOLOCK;
#endif

#endif


