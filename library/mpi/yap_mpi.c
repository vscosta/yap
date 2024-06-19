/*
  Copyright (C) 2004,2005,2006 (Nuno A. Fonseca) <nuno.fonseca@gmail.com>

  This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WxuARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

`You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


Last rev: $Id: yap_mpi.c,v 1.4 2006-09-28 11:42:51 vsc Exp $
Comments: YAP interface to LAM/MPI
 */

#include <stdio.h>
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_MALLOC_H

#include <malloc.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif

#if HAVE_MPI_H
#include <mpi.h>

#include "prologterms2c.h"

#include <YapInterface.h>

#include "hash.h"

/*********************************************************************/
struct broadcast_req {
  void
      *ptr; // pointer to an allocated memory buffer associated to the broadcast
  int nreq; // number of requests associated to the broadcast
};
typedef struct broadcast_req BroadcastRequest;

/*********************************************************************/
#define IDTYPE long
#define HANDLE2INT(ptr) (IDTYPE) ptr
#define INT2HANDLE(id) (MPI_Request *)id

#define BREQ2INT(ptr) (IDTYPE) ptr
#define INT2BREQ(ptr) (BroadcastRequest *)ptr

#define MPI_CALL(function)                                                     \
  ((mpi_status = function) == MPI_SUCCESS ? MPI_SUCCESS : mpi_error(mpi_status))

#ifdef USE_THREADS
#include <pthread.h>
#endif

#define BUF_INITIAL_SIZE 4096
  

/********************************************************************
 * Auxiliary data
 ********************************************************************/
#ifdef USE_THREADS
  static YAP_Bool mpi_status;
#else
  static YAP_Bool mpi_statuss[1024];
#define mpi_status (mpi_statuss[YAP_ThreadSelf()])
#endif

extern int GLOBAL_argc;
extern char **GLOBAL_argv;

X_API void init_mpi(void);

/********************************************************************
 * Time accounting
 ********************************************************************/
#ifdef MPISTATS

#include <sys/time.h>
#include <time.h>

/* Statistics */
static unsigned long bytes_sent;     // bytes received (mpi headers are ignored)
static unsigned long bytes_recv;     // bytes received
static unsigned long num_msgs_sent;  // number of messages sent
static unsigned long num_msgs_recv;  // number of messages received
static unsigned long max_s_recv_msg; // maximum size of a message received
static unsigned long max_s_sent_msg; // maximum size of a message sent
static double total_time_spent;      // total time spend in communication code

/* MSG ACCOUNTING */
#define RESET_STATS()                                                          \
  {                                                                            \
  total_time_spent = 0;                                                      \
  bytes_sent = bytes_recv = num_msgs_recv = num_msgs_sent = max_s_recv_msg = \
  max_s_sent_msg = 0;                                                    \
  }

#define MSG_SENT(size)                                                         \
  {                                                                            \
  bytes_sent += size;                                                        \
  ++num_msgs_sent;                                                           \
  if (max_s_sent_msg < size)                                                 \
  max_s_sent_msg = size;                                                   \
  }
#define MSG_RECV(size)                                                         \
  {                                                                            \
  bytes_recv += size;                                                        \
  ++num_msgs_recv;                                                           \
  if (max_s_recv_msg < size)                                                 \
  max_s_recv_msg = size;                                                   \
  }

#define MPITIME total_time_spent

/* Timer */
#define CONT_TIMER()                                                           \
  { tstart(); }
#define PAUSE_TIMER()                                                          \
  {                                                                            \
  tend();                                                                    \
  total_time_spent += tval();                                                \
  }

#define return(p)                                                              \
  {                                                                            \
  PAUSE_TIMER();                                                             \
  return (p);                                                                \
  }

#if USE_THREADS
  static struct timeval _tstarts[1024], _tends[1024];
#define _tsart (_tstarts[YAP_ThreadSelf()])
#define _tend (_tends[YAP_ThreadSelf()])
#else
  static struct timeval tstart_, _tend;
#endif
#include <sys/fffffftffime.h>
#include <sys/resource.h>
#include <unistd.h>

void tstart(void) {
  struct rusage r;
  getrusage(RUSAGE_SELF, &r);
  _tstart = r.ru_utime;
}
void tend(void) {
  struct rusage r;
  getrusage(RUSAGE_SELF, &r);
  _tend = r.ru_utime;
}
//
double tval() {
  double t1, t2, elapsed;
  t1 = (double)_tstart.tv_sec + (double)_tstart.tv_usec / (1000 * 1000);
  t2 = (double)_tend.tv_sec + (double)_tend.tv_usec / (1000 * 1000);
  elapsed = t2 - t1;
  if (elapsed == 0)
    return 0.000001;
  return elapsed;
}
/*
 * returns the statistics
 */
static YAP_Bool mpi_stats(void) {
  fprintf(stderr, "%f  %ld %ld %ld %ld %ld %ld\n", MPITIME, num_msgs_recv,
          bytes_recv, max_s_recv_msg, num_msgs_sent, bytes_sent,
          max_s_sent_msg);
  return (YAP_Unify(YAP_ARG1, YAP_MkFloatTerm((float)(MPITIME))) &&
          YAP_Unify(YAP_ARG2, YAP_MkIntTerm((long)num_msgs_recv)) &&
          YAP_Unify(YAP_ARG3, YAP_MkIntTerm((long)bytes_recv)) &&
          YAP_Unify(YAP_ARG4, YAP_MkIntTerm((long)max_s_recv_msg)) &&
          YAP_Unify(YAP_ARG5, YAP_MkIntTerm((long)num_msgs_sent)) &&
          YAP_Unify(YAP_ARG6, YAP_MkIntTerm((long)bytes_sent)) &&
          YAP_Unify(YAP_ARG7, YAP_MkIntTerm((long)max_s_sent_msg)));
}
/*
 *
 */
static YAP_Bool mpi_reset_stats(void) {
  RESET_STATS();
  return true;
}
#else

#define PAUSE_TIMER()
#define CONT_TIMER()

#define RESET_STATS()
#define MSG_SENT(size)
#define MSG_RECV(size)
#define return(p)                                                              \
  { return (p); }
#endif

/********************************************************************
 * Functions to store/fetch/delete requests
 ********************************************************************/

typedef struct req__ {
  MPI_Request req;
  char *buf;
  bool export;
} req_t;

static inline YAP_UInt new_request(MPI_Request *handle, void *ptr, bool export) {
  struct req__ *r;
  r = malloc(sizeof *r);
  if (handle)
    r->req = *handle;
  r->buf = ptr;
  r->export = export;
  return ((YAP_UInt)r);
}

static inline void free_request(YAP_UInt v) {
  struct req__ *r = (struct req__ *)v;
  if (r->export && r->buf)
    free(r->buf);
  free(r);
}

static inline MPI_Request *get_request(YAP_UInt v) {
  struct req__ *r = (struct req__ *)v;
  return &(r->req);
}

static inline void *get_buffer(YAP_UInt v) {
  struct req__ *r = (struct req__ *)v;
  return (r->buf);
}

/*********************************************************************/
static YAP_Bool mpi_error(int errcode) {
  char err_msg[MPI_MAX_ERROR_STRING];
  int len;

  MPI_Error_string(errcode, &err_msg[0], &len);
  err_msg[len] = '\0';
#ifdef MPI_DEBUG
  write_msg(__FUNCTION__, __FILE__, __LINE__, "MPI_Error: %s\n", err_msg);
#endif
  return errcode;
}
/********************************************************************/

/********************************************************************/
/**
 * @pred mpi_init
 * Sets up the mpi enviromment. This function should be called before any other
 * MPI function.
 */
static bool initialized = false;

static YAP_Bool 
mpi_init(void){
  if (initialized)
    return true;
#if USE_THREADS
  int thread_level;
  char ** my_argv;
  //int my_argc = YAP_Argv(&my_argv);
  //  MPI_Init_thread(&my_argc, &my_argv, MPI_THREAD_SINGLE, &thread_level);
#else
  MPI_Init(&GLOBAL_argc, &GLOBAL_argv);
#endif
#ifdef MPI_DEBUG
  write_msg(__FUNCTION__, __FILE__, __LINE__, "Thread level: %d\n",
            thread_level);
#endif
#ifdef MPISTATS
  RESET_STATS();
#endif
  initialized = true;
  return true;
}

#ifdef USE_THREADS
  /*
   * Sets up the mpi enviromment. This function should be called before any other
   * MPI function. the argument is the name of the predicate that will be invoked
   * when a message is received
   */
  static YAP_Bool rcv_msg_thread(char *handle_pred) {
    YAP_Term pred = YAP_MkAtomTerm(YAP_LookupAtom(handle_pred));
    MPI_Status status;

    while (1) {
      write_msg(__FUNCTION__, __FILE__, __LINE__, "Waiting for MPI msg\n");
      if (MPI_CALL(MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD,
                             &status)) == MPI_SUCCESS) {
	// call handle
	write_msg(__FUNCTION__, __FILE__, __LINE__, "MPI Msg received\n");
	YAP_RunGoal(pred);
      } else
	write_msg(__FUNCTION__, __FILE__, __LINE__, "Error in MPI_Probe\n");
    }
    return 1;
  }
  /*
   *
   */
  static YAP_Bool mpi_init_rcv_thread(void) {
    int thread_level;
    //  MPI_Init(&GLOBAL_argc, &GLOBAL_argv);
    pthread_t thread;
    char *arg = "handle_msg";

    MPI_Init_thread(&GLOBAL_argc, &GLOBAL_argv, MPI_THREAD_SINGLE, &thread_level);
    if (pthread_create(&thread, NULL, (void *)&rcv_msg_thread, arg)) {
      return false;
    }

    pthread_detach(thread);
    write_msg(__FUNCTION__, __FILE__, __LINE__, "Thread level: %d\n",
              thread_level);
    return true;
  }
#endif

/*
 *Terminates the MPI execution enviroment. Every process must call this function
 *before exiting. mpi_comm_finalize.
 */
static YAP_Bool mpi_finalize(void) {
  return (MPI_Finalize() == MPI_SUCCESS ? true : false);
}
/*
 * Returns the number of workers associated to the MPI_COMM_WORLD communicator.
 * mpi_comm_size(-Size).
 */
static YAP_Bool mpi_comm_size(void) {
  int size;
  MPI_CALL(MPI_Comm_size(MPI_COMM_WORLD, &size));
  return (YAP_Unify(YAP_ARG1, YAP_MkIntTerm(size)));
}
/*
 * Returns the rank of the current process.
 * mpi_comm_rank(-Rank).
 */
static YAP_Bool mpi_comm_rank(void) {
  int rank;
  MPI_CALL(MPI_Comm_rank(MPI_COMM_WORLD, &rank));
  return (YAP_Unify(YAP_ARG1, YAP_MkIntTerm(rank)));
}
/*
 * Returns the major and minor version of MPI.
 *  mpi_version(-Major,-Minor).
 */
static YAP_Bool mpi_version(void) {
  int major, minor;

  MPI_CALL(MPI_Get_version(&major, &minor));
  return (YAP_Unify(YAP_ARG1, YAP_MkIntTerm(major)) &&
          YAP_Unify(YAP_ARG2, YAP_MkIntTerm(minor)));
}
/*
 *
 *
 */
static YAP_Bool mpi_get_processor_name(void) {
  char name[MPI_MAX_PROCESSOR_NAME];
  int length;
  MPI_CALL(MPI_Get_processor_name(name, &length));
  return (YAP_Unify(YAP_ARG1, YAP_MkAtomTerm(YAP_LookupAtom(name))));
}
/*
 * Non blocking communication function. The message is sent when possible. To
 * check for the status of the message, the mpi_wait and mpi_test should be
 * used. Until mpi_wait is called, the memory allocated for the buffer
 * containing the message is not released.
 *
 * mpi_isend(+Data, +Destination, +Tag, -Handle).
 */
static YAP_Bool mpi_isend(void) {
  YAP_Term t1 = YAP_Deref(YAP_ARG1), t2 = YAP_Deref(YAP_ARG2),
           t3 = YAP_Deref(YAP_ARG3), t4 = YAP_Deref(YAP_ARG4);
  int dest, tag;
  CONT_TIMER();

  if (YAP_IsVarTerm(t1) || !YAP_IsIntTerm(t2) || !YAP_IsIntTerm(t3) ||
      !YAP_IsVarTerm(t4)) {
    PAUSE_TIMER();
    return false;
  }
  //
  dest = YAP_IntOfTerm(t2);
  tag = YAP_IntOfTerm(t3);
  //
  char *str = term2string(t1);
  size_t len = strlen(str) + 1;
  // send the data
  YAP_UInt r = new_request(NULL, str, false);
  if (MPI_CALL(MPI_Isend(str, len, MPI_CHAR, dest, tag, MPI_COMM_WORLD,
                         get_request(r))) != MPI_SUCCESS) {
    PAUSE_TIMER();
    return false;
  }
  MSG_SENT(len);

#ifdef MPI_DEBUG
  write_msg(__FUNCTION__, __FILE__, __LINE__, "%s(%s,%u, MPI_CHAR,%d,%d)\n",
            __FUNCTION__, str, len, dest, tag);
#endif
  // USED_BUFFER(); //  informs the prologterm2c module that the buffer is now
  // used and should not be messed
  //  We must associate the string to each handle
  PAUSE_TIMER();
  return (YAP_Unify(YAP_ARG4, YAP_MkIntTerm(r)));
}

/*
 * Blocking communication function. The message is sent immediatly.
 * mpi_send(+Data, +Destination, +Tag).
 */
static YAP_Bool mpi_send(void) {

  YAP_Term t1 = YAP_Deref(YAP_ARG1), t2 = YAP_Deref(YAP_ARG2),
           t3 = YAP_Deref(YAP_ARG3);
  char *str = NULL;
  int dest, tag;
  size_t len;
  int val;
  if (YAP_IsVarTerm(t1) || !YAP_IsIntTerm(t2) || !YAP_IsIntTerm(t3)) {
    return false;
  }

  CONT_TIMER();
  //
  dest = YAP_IntOfTerm(t2);
  tag = YAP_IntOfTerm(t3);
  // the data is packaged as a string
  str = term2string(t1);
  len = strlen(str) + 1;
#if defined(MPI_DEBUG)
  write_msg(__FUNCTION__, __FILE__, __LINE__, "%s(%s,%u, MPI_CHAR,%d,%d)\n",
            __FUNCTION__, str, len, dest, tag);
#endif
  // send the data
  val = (MPI_CALL(MPI_Send(str, len, MPI_CHAR, dest, tag, MPI_COMM_WORLD)) ==
                 MPI_SUCCESS
             ? true
             : false);

  PAUSE_TIMER();
  return (val);
}
/*
 * Implements a blocking receive operation.
 *  mpi_recv(?Source,?Tag,-Data).
 */
static YAP_Bool mpi_recv(void) {
  YAP_Term t1 = YAP_Deref(YAP_ARG1), t2 = YAP_Deref(YAP_ARG2), t4;
  int tag, orig;
  MPI_Status status;
  char *tmp = malloc(BLOCK_SIZE);
  /* The first argument (Source) must be bound to an integer
     (the rank of the source) or left unbound (i.e. any source
     is OK) */
  if (YAP_IsVarTerm(t1))
    orig = MPI_ANY_SOURCE;
  else if (!YAP_IsIntTerm(t1))
    return false;
  else
    orig = YAP_IntOfTerm(t1);

  /* The second argument must be bound to an integer (the tag)
     or left unbound (i.e. any tag is OK) */
  if (YAP_IsVarTerm(t2))
    tag = MPI_ANY_TAG;
  else if (!YAP_IsIntTerm(t2))
    return false;
  else
    tag = YAP_IntOfTerm(t2);
  CONT_TIMER();
  // probe for term' size
  if (MPI_CALL(MPI_Probe(orig, tag, MPI_COMM_WORLD, &status)) != MPI_SUCCESS) {
    PAUSE_TIMER();
    return false;
  }
  int count;
  if (MPI_CALL(MPI_Get_count(&status, MPI_CHAR, &count)) != MPI_SUCCESS ||
      status.MPI_TAG == MPI_UNDEFINED || status.MPI_SOURCE == MPI_UNDEFINED) {
    PAUSE_TIMER();
    return false;
  }
  // realloc memory buffer
  char *buf = tmp;
  if (count > BLOCK_SIZE)
    buf = malloc(count);
  // Receive the message as a stringp
  if (MPI_CALL(MPI_Recv(buf, count, MPI_CHAR, orig, tag, MPI_COMM_WORLD,
                        MPI_STATUS_IGNORE)) != MPI_SUCCESS) {
    /* Getting in here should never happen; it means that the first
       package (containing size) was sent properly, but there was a glitch with
       the actual content! */
    PAUSE_TIMER();
    if (tmp != buf)
      free(buf);
    return false;
  }
#ifdef MPI_DEBUG
  write_msg(__FUNCTION__, __FILE__, __LINE__, "%s(%s,%u, MPI_CHAR,%d,%d)\n",
            __FUNCTION__, buf, count, orig, tag);
#endif
  MSG_RECV(count);
  t4 = string2term(buf, NULL);
  PAUSE_TIMER();
  if (tmp != buf)
    free(buf);
  return (YAP_Unify(YAP_ARG3, t4));
}

/*
 * Implements a non-blocking receive operation.
 * mpi_irecv(?Source,?Tag,-Handle).
 */
static YAP_Bool mpi_irecv(void) {
  YAP_Term t1 = YAP_Deref(YAP_ARG1), t2 = YAP_Deref(YAP_ARG2),
           t3 = YAP_Deref(YAP_ARG3);
  int tag, orig;

  // The third argument (data) must be unbound
  /* The first argument (Source) must be bound to an integer
     (the rank of the source) or left unbound (i.e. any source
     is OK) */
  if (YAP_IsVarTerm(t1))
    orig = MPI_ANY_SOURCE;
  else if (!YAP_IsIntTerm(t1))
    return false;
  else
    orig = YAP_IntOfTerm(t1);

  /* The third argument must be bound to an integer (the tag)
     or left unbound (i.e. any tag is OK) */
  if (YAP_IsVarTerm(t2))
    tag = MPI_ANY_TAG;
  else if (!YAP_IsIntTerm(t2))
    return false;
  else
    tag = YAP_IntOfTerm(t2);
  /* The third argument must be bound to an integer (the tag)
     or left unbound (i.e. any tag is OK) */
  int sz;
  if (!YAP_IsIntTerm(t3))
    return false;
  else
    sz = YAP_IntOfTerm(t3);

  CONT_TIMER();
  char *tmp = malloc(sz);
  YAP_UInt r = new_request(NULL, tmp, true);
  if (MPI_CALL(MPI_Irecv(tmp, sz, MPI_CHAR, orig, tag, MPI_COMM_WORLD,
                         get_request(r))) != MPI_SUCCESS) {
    PAUSE_TIMER();
    return false;
  }

  PAUSE_TIMER();
  return YAP_Unify(YAP_ARG4, YAP_MkIntTerm(r));
}


/** @pred mpi_wait(? _Handle_)
 *
 *  Completes a non-blocking operation. IF the operation was a send, the
 * function waits until the message is buffered or sent by the runtime
 * system. At this point the send buffer is released. If the operation
 * was a receive, it waits until the message is copied to the receive
 * buffer.
 * 
 */
static YAP_Bool mpi_wait(void) {
  YAP_Term t1 = YAP_Deref(YAP_ARG1);  // handle
  MPI_Status status;
  MPI_Request *handle;
  // The first argument  must be an integer (an handle)
  if (!YAP_IsIntTerm(t1)) {
    return false;
  }
  handle = get_request(YAP_IntOfTerm(t1));
  CONT_TIMER();
  // probe for term' size
  if (MPI_CALL(MPI_Wait(handle, &status)) != MPI_SUCCESS) {
    PAUSE_TIMER();
    return false;
  }
  free_request(YAP_IntOfTerm(t1));
  PAUSE_TIMER();
  return true;
}

/*
 * @pred mpi_test(+Handle)
 *
 *  Provides information regarding a handle, ie. if a communication operation
 * has been completed. If the operation has been completed the predicate
 * succeeds, otherwise it fails.
 * ).
 */
static YAP_Bool mpi_test(void) {
  YAP_Term t1 = YAP_Deref(YAP_ARG1);      // Status
  MPI_Request *handle;
  MPI_Status status;
  int flag;

  // The first argument (handle) must be an integer
  if (!YAP_IsIntTerm(t1)) {
    return false;
  }
  CONT_TIMER();

  handle = get_request(YAP_IntOfTerm(t1));
  //
  MPI_CALL(MPI_Test(handle, &flag, &status));
  if (flag != true) {
    PAUSE_TIMER();
    return false;
  }
  free_request(YAP_IntOfTerm(t1));
  PAUSE_TIMER();
  return true;
}

/** @mpi_wait (+Handle,-Data)
 *
 *  Completes a non-blocking operation. IF the operation was a send, the
 * function waits until the message is buffered or sent by the runtime
 * system. At this point the send buffer is released. If the operation
 * was a receive, it waits until the message is copied to the receive
 * buffer, and then unifies the result with _Data_.
 *
 */
static YAP_Bool mpi_wait_recv(void) {
  YAP_Term t1 = YAP_Deref(YAP_ARG1); // data
  MPI_Request *handle;
  MPI_Status status;
  char *s;
  if (!YAP_IsIntTerm(t1)) {
    return false;
  }
  CONT_TIMER();

  handle = get_request(YAP_IntOfTerm(t1));
  s = get_buffer(YAP_IntOfTerm(t1));
  // wait for communication completion
  if (MPI_CALL(MPI_Wait(handle, &status)) != MPI_SUCCESS) {
    PAUSE_TIMER();
    return false;
  }
  YAP_Bool ret = true;
  // make sure we only fetch ARG3 after constructing the term
  YAP_Term out = string2term(s, NULL);
  MSG_RECV(len);
  free_request(YAP_IntOfTerm(t1));
  PAUSE_TIMER();
  ret = YAP_Unify(YAP_ARG2, out);
  return ret;
}

/** @pred mpi_test(? _Handle_,- _Data_)
 *
 *
 * Provides information regarding a handle, ie. if a communication operation has
 * been completed. If the operation has been completed the predicate succeeds
 * with the completion status, otherwise it fails.
 *
 */
static YAP_Bool mpi_test_recv(void) {
  YAP_Term t1 = YAP_Deref(YAP_ARG1); // data

  MPI_Status status;
  MPI_Request *handle;
  int flag, len, ret;
  char *s;
  YAP_Term out;

  // The first argument (handle) must be an integer
  if (!YAP_IsIntTerm(t1)) {
    return false;
  }
  CONT_TIMER();

  handle = get_request(YAP_IntOfTerm(t1));
  //
  if (MPI_CALL(MPI_Test(handle, &flag, &status)) != MPI_SUCCESS) {
    PAUSE_TIMER();
    return false;
  }
  s = get_buffer(YAP_IntOfTerm(t1));
  len = strlen(s);
  out = string2term(s, (size_t *)&len);
  // make sure we only fetch ARG3 after constructing the term
  ret = YAP_Unify(YAP_ARG3, out);
  free_request(YAP_IntOfTerm(t1));
  PAUSE_TIMER();
  return ret;
}

/**
* @pred mpi_barrier
*
* Collective communication function that performs a barrier synchronization
 * among all processes. mpi_barrier
 */
static YAP_Bool mpi_barrier(void) {
  CONT_TIMER();
  int ret = MPI_CALL(MPI_Barrier(MPI_COMM_WORLD));
  PAUSE_TIMER();
  return (ret == MPI_SUCCESS ? true : false);
}

/** @pred mpi_bcast(+ _Root_, +_BufSize_, ? _Data_)
 *
 * Broadcasts the message  _Data_ from the process with rank  _Root_
to all other processes.
*/
static YAP_Bool mpi_bcast(void) {
  YAP_Term t1 = YAP_Deref(YAP_ARG1),
 t2 = YAP_Deref(YAP_ARG2) ,
 t3 = YAP_Deref(YAP_ARG3);
  char *str = NULL;
  size_t len;
  int val;
  int rank;
  if (!YAP_IsIntTerm(t1)) {
    return false;
  }

  CONT_TIMER();
  //
  int root = YAP_IntOfTerm(t1);
  // the data is packaged as a string

  if (MPI_Comm_rank(MPI_COMM_WORLD, &rank) == root) {
    str = term2string(t2);
    len = strlen(str) + 1;
  } else {
  if (YAP_IsVarTerm(t2)) {
    len =BUF_INITIAL_SIZE;
  } else {
    len = YAP_IntOfTerm(t2);
  }
  str = malloc(len);
  }
#if defined(MPI_DEBUG)
  write_msg(__FUNCTION__, __FILE__, __LINE__, "%s(%s,%u, MPI_CHAR,%d,%d)\n",
            __FUNCTION__, str, len, dest, tag);

#endif
  // send the data
  if (MPI_Comm_rank(MPI_COMM_WORLD, &rank) == root) {
    str = term2string(t3);
    len = strlen(str) + 1;
  } else {
    if (YAP_IsVarTerm(t2)) {
    len =BUF_INITIAL_SIZE;
  } else {
    len = YAP_IntOfTerm(t2);
  }
  str = malloc(len);
  }
  val = (MPI_CALL(MPI_Bcast(str, len, MPI_CHAR, root, MPI_COMM_WORLD)) ==
                 MPI_SUCCESS
             ? true
         : false);
  if (!val) return false;
  if ( rank!= root) {
    Term out = string2term(str, (size_t *)&len);
    // make sure we only fetch ARG3 after constructing the term
    free(str);
    val = YAP_Unify(YAP_ARG3, out);
  }

  PAUSE_TIMER();
  return (val);
}


                    /** @pred mpi_ibcast(+ _Root_, + _Data_, + _Tag_)
                       
                       
                       
                         Non-blocking operation. Broadcasts the message  _Data_
                         from the process with rank  _Root_ to all other processes.
                       
                       
                         */
static YAP_Bool mpi_ibcast(void) {
  YAP_Term t1 = YAP_Deref(YAP_ARG1),
 t3 = YAP_Deref(YAP_ARG3),
 t2 = YAP_Deref(YAP_ARG2);
  size_t len;
  int root;
  char *str;
  CONT_TIMER();

  if (!YAP_IsIntTerm(t1)) {
    PAUSE_TIMER();
    return false;
  }
  //
  root = YAP_IntOfTerm(t1);
  int rank;
  //
  // send the data
  // send the data
  if (MPI_Comm_rank(MPI_COMM_WORLD, &rank) == root) {
    str = term2string(t3);
    len = strlen(str) + 1;
  } else {
  if (YAP_IsVarTerm(t2)) {
    len =BUF_INITIAL_SIZE;
  } else {
    len = YAP_IntOfTerm(t2);
  }
  str = malloc(len);
  }
  YAP_UInt r = new_request(NULL, str, false);
  if (MPI_CALL(MPI_Ibcast(str, len, MPI_CHAR, root, MPI_COMM_WORLD,
                         get_request(r))) != MPI_SUCCESS) {
    PAUSE_TIMER();
    return false;
  }
  MSG_SENT(len);

#ifdef MPI_DEBUG
  write_msg(__FUNCTION__, __FILE__, __LINE__, "%s(%s,%u, MPI_CHAR,%d,%d)\n",
            __FUNCTION__, str, len, dest, tag);
#endif
  // USED_BUFFER(); //  informs the prologterm2c module that the buffer is now
  // used and should not be messed
  //  We must associate the string to each handle
  PAUSE_TIMER();
  return (YAP_Unify(YAP_ARG4 , YAP_MkIntTerm(r)));

}
/*******************************************
 * Buffer Allocation */

/**
 * @pred mpi_default_buffer_size(-Sz)
 *
 * Tell the size of the default message buffer.
 */
static YAP_Bool mpi_default_buffer_size(void) {
  if (!YAP_Unify(YAP_ARG1, YAP_MkIntTerm(BLOCK_SIZE))) {
    return false;
  }
  return true;
}

/**
 * @pred mpi_term_to_buffer_size(-Sz)
 *
 * Tell the size of required for a term message.
 *
 */
static YAP_Bool mpi_buffer_size(void) {
   char *str = term2string (YAP_ARG1);
  size_t len = strlen(str) + 1;

  return YAP_Unify(YAP_ARG2, YAP_MkIntTerm(len));
}

static YAP_Bool mpi_stop(void) {
  volatile bool stop = true;
  while (stop);
  return true;
}

/********************************************************************
 * Init
 *******************************************************************/
X_API void init_mpi(void) {
  YAP_SetYAPFlag(YAP_MkAtomTerm(YAP_LookupAtom("readline")),
                 YAP_MkAtomTerm(YAP_LookupAtom("false")));
  YAP_UserCPredicate("mpi_init", mpi_init, 0); // mpi_init/0
  YAP_UserCPredicate("mpi_finalize", mpi_finalize, 0); // mpi_init/0
  YAP_UserCPredicate("mpi_stop", mpi_stop, 0); // mpi_init/0
  YAP_UserCPredicate("mpi_version", mpi_version, 2); // mpi_init/0
  YAP_UserCPredicate("mpi_comm_rank", mpi_comm_rank, 1); // mpi_init/0
  YAP_UserCPredicate("mpi_comm_size", mpi_comm_size, 1); // mpi_init/0
#ifdef USE_THREADS
  YAP_UserCPredicate("mpi_init_rcv_thread", mpi_init_rcv_thread,
                     1); // mpi_init_rcv_thread(+Handle, 4)t;
#endif
  YAP_UserCPredicate("mpi_send", mpi_send, 3); // mpi_recv(?Source,?Tag,-Data).
  YAP_UserCPredicate("mpi_isend", mpi_isend, 4); // mpi_recv(?Source,?Tag,-Data).
  YAP_UserCPredicate("mpi_get_processor_name", mpi_get_processor_name, 1); // mpi_recv(?Source,?Tag,-Data).
  YAP_UserCPredicate("mpi_recv", mpi_recv, 3); // mpi_recv(?Source,?Tag,-Data).
  YAP_UserCPredicate("mpi_irecv", mpi_irecv,
                     4); // mpi_irecv(?Source,?Tag,-Handle).
  YAP_UserCPredicate("mpi_wait", mpi_wait, 1);// mpi_wait(+Handle).
  YAP_UserCPredicate("mpi_wait", mpi_wait_recv,
                     2); // mpi_wait_recv(+Handle,-Data).
  YAP_UserCPredicate("mpi_test", mpi_test, 1); // mpi_test(+Handle).
  YAP_UserCPredicate("mpi_test", mpi_test_recv,
                     2); // mpi_test(+Handle,-Data).
  YAP_UserCPredicate("mpi_bcast", mpi_bcast, 3);  // mpi_bcast(Root,Term)
  YAP_UserCPredicate("mpi_ibcast", mpi_ibcast, 4); // mpi_bcast3(Root,Term,Handle)
  YAP_UserCPredicate("mpi_barrier", mpi_barrier, 0); // mpi_barrier/0
  YAP_UserCPredicate("mpi_default_buffer_size", mpi_default_buffer_size,
                     1); // buffer size
  YAP_UserCPredicate("mpi_buffer_size", mpi_buffer_size,
                     2); // buffer size

#ifdef MPISTATS
  YAP_UserCPredicate(
      "mpi_stats", mpi_stats,
      7); // mpi_stats(-Time,#MsgsRecv,BytesRecv,MaxRecev,#MsgSent,BytesSent,MaxSent)
  YAP_UserCPredicate("mpi_reset_stats", mpi_reset_stats,
                     0); // cleans the timers
  RESET_STATS();
#endif
  //  YAP_UserCPredicate( "mpi_gather", mpi_gather,0);
  //  //mpi_gather(+RootRank,?SendData,?RecvData)
  // Each process (root process included) sends the contents of its send buffer
  // to the root process. The root process receives the messages and stores them
  // in rank order. The outcome is  as if each of the  n processes in the group
  // (including the root process) had executed a call to MPI_Send and the root
  // had executed n calls to MPI_Recv.  The receive buffer is ignored for all
  // non-root processes. MPI_Scatter
#ifdef MPI_DEBUG
  fprintf(stderr, "MPI  module succesfully loaded.");
  fflush(stderr);
#endif
}

#endif /* HAVE_MPI_H */
