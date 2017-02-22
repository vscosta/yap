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

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


Last rev: $Id: yap_mpi.c,v 1.4 2006-09-28 11:42:51 vsc Exp $
Comments: YAP interface to LAM/MPI
*/
#include "config.h"
#include <stdlib.h>
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
  void *ptr; // pointer to an allocated memory buffer associated to the broadcast
  int  nreq; // number of requests associated to the broadcast
};
typedef struct broadcast_req BroadcastRequest;

/*********************************************************************/
#define  IDTYPE long
#define  HANDLE2INT(ptr) (IDTYPE)ptr
#define  INT2HANDLE(id)  (MPI_Request*)id

#define BREQ2INT(ptr)  (IDTYPE)ptr
#define INT2BREQ(ptr)  (BroadcastRequest*)ptr

#define MPI_CALL(function) ((mpi_status=function)==MPI_SUCCESS?MPI_SUCCESS:mpi_error(mpi_status))

#ifdef USE_THREADS
#include <pthread.h>
#endif

/********************************************************************
 * Auxiliary data
 ********************************************************************/
static YAP_Bool mpi_statuss[1024];
#define mpi_status (mpi_statuss[YAP_ThreadSelf()])

extern int GLOBAL_argc;

#define HASHSIZE 1777
static hashtable requests=NULL;
static hashtable broadcasts=NULL;

X_API void init_mpi(void);

/********************************************************************
 * Time accounting
 ********************************************************************/
#ifdef MPISTATS

#include <sys/time.h>
#include <time.h>

/* Statistics */
static unsigned long bytes_sent;    // bytes received (mpi headers are ignored)
static unsigned long bytes_recv;    // bytes received
static unsigned long num_msgs_sent; // number of messages sent
static unsigned long num_msgs_recv; // number of messages received
static unsigned long max_s_recv_msg;// maximum size of a message received 
static unsigned long max_s_sent_msg;// maximum size of a message sent
static double total_time_spent;     // total time spend in communication code

/* MSG ACCOUNTING */
#define RESET_STATS()        {total_time_spent=0;bytes_sent=bytes_recv=num_msgs_recv=num_msgs_sent=max_s_recv_msg= max_s_sent_msg=0;}
#define MSG_SENT(size)       {bytes_sent+=size;++num_msgs_sent;if(max_s_sent_msg<size)max_s_sent_msg=size;}
#define MSG_RECV(size)       {bytes_recv+=size;++num_msgs_recv;if(max_s_recv_msg<size)max_s_recv_msg=size;}

#define MPITIME         total_time_spent

/* Timer */
#define CONT_TIMER()        {tstart();}
#define PAUSE_TIMER()       {tend();total_time_spent+=tval();}

#define return(p)     {PAUSE_TIMER();return (p);}

static struct timeval _tstarts[1024], _tends[1024];

#define _tsart (_tstarts[YAP_ThreadSelf()])
#define _tend  (_tends[YAP_ThreadSelf()])

#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

void tstart(void) {
  struct rusage r;
  getrusage(RUSAGE_SELF,&r);
  _tstart=r.ru_utime;
}
void tend(void) {
  struct rusage r;
  getrusage(RUSAGE_SELF,&r);
  _tend=r.ru_utime;
}
//
double tval(){
  double t1, t2,elapsed;   
  t1 =  (double)_tstart.tv_sec + (double)_tstart.tv_usec/(1000*1000); 
  t2 =  (double)_tend.tv_sec + (double)_tend.tv_usec/(1000*1000); 
  elapsed=t2-t1;
  if (elapsed==0) return 0.000001;
  return elapsed;
}
/*
 * returns the statistics
 */
static YAP_Bool  mpi_stats(void){  
  fprintf(stderr,"%f  %ld %ld %ld %ld %ld %ld\n",MPITIME,num_msgs_recv,bytes_recv,max_s_recv_msg,num_msgs_sent,bytes_sent,max_s_sent_msg);
  return (YAP_Unify(YAP_ARG1, YAP_MkFloatTerm((float)(MPITIME))) && 
	  YAP_Unify(YAP_ARG2, YAP_MkIntTerm((long)num_msgs_recv))      &&
	  YAP_Unify(YAP_ARG3, YAP_MkIntTerm((long)bytes_recv))         &&
	  YAP_Unify(YAP_ARG4, YAP_MkIntTerm((long)max_s_recv_msg))     &&
	  YAP_Unify(YAP_ARG5, YAP_MkIntTerm((long)num_msgs_sent))      &&
	  YAP_Unify(YAP_ARG6, YAP_MkIntTerm((long)bytes_sent))         &&
	  YAP_Unify(YAP_ARG7, YAP_MkIntTerm((long)max_s_sent_msg))
	  );
}
/*
 * 
 */
static YAP_Bool  mpi_reset_stats(void) {RESET_STATS(); return  true;}
#else

#define PAUSE_TIMER()
#define CONT_TIMER()


#define RESET_STATS()        
#define MSG_SENT(size)       
#define MSG_RECV(size)       
#define return(p)     {return (p);}
#endif

/********************************************************************
 * Functions to store/fetch/delete requests
 ********************************************************************/

static inline int 
new_request(MPI_Request *handle,void* ptr) {
  return insere(requests,(ulong)HANDLE2INT(handle),ptr);
}
 
static inline void* 
get_request(MPI_Request *handle) {
  return get_object(requests,(ulong)HANDLE2INT(handle));
} 

static inline void 
free_request(MPI_Request *handle) {
  void* ptr;
  ptr=delete(requests,(ulong)HANDLE2INT(handle));
  free(ptr);
  free(handle);
}
/********************************************************************
 * Functions to store/fetch/delete broadcast requests
 ********************************************************************/
/*
 * Returns a new BroadcastRequest object
 */
static inline BroadcastRequest* 
new_broadcast(void) {
  BroadcastRequest* b=(BroadcastRequest *)malloc(sizeof(BroadcastRequest));
  if ( b!=NULL) {
    b->ptr=NULL;
    b->nreq=0;
  }
  //  write_msg(__FUNCTION__,__FILE__,__LINE__,"new broadcast: %p\n",b);
  return b;
}
/*
 *
 */
static inline void 
free_broadcast_request(MPI_Request *handle) {
  BroadcastRequest* b;
  b=(BroadcastRequest*)delete(broadcasts,(ulong)BREQ2INT(handle));// get the ptr to broadcast object
  b->nreq--;
  if ( !b->nreq ) {
    // all requests received
    free(b->ptr);
    free(b);
  }
  //  write_msg(__FUNCTION__,__FILE__,__LINE__,"free broadcast_request: %p->%p\n",b,handle);
  free(handle);
}
/*
 *
 */
static inline void* 
get_broadcast_request(MPI_Request *handle) {
  return get_object(broadcasts,(ulong)HANDLE2INT(handle));
} 
/*
 *
 */
static inline int 
new_broadcast_request(BroadcastRequest* b,MPI_Request *handle,void* ptr) {
  b->ptr=ptr;
  b->nreq++;
  //write_msg(__FUNCTION__,__FILE__,__LINE__,"new broadcast_request: %p->%p\n",b,handle);
  return insere(broadcasts,(ulong)HANDLE2INT(handle),b);
}

/*********************************************************************/
static YAP_Bool mpi_error(int errcode){
  char err_msg[MPI_MAX_ERROR_STRING];
  int len;
  
  MPI_Error_string(errcode,&err_msg[0],&len);
  err_msg[len]='\0';
#ifdef DEBUG
  write_msg(__FUNCTION__,__FILE__,__LINE__,"MPI_Error: %s\n",err_msg);
#endif
  return errcode;
}
/********************************************************************

 ********************************************************************/
/*
 * Sets up the mpi enviromment. This function should be called before any other MPI
 * function.
 */
static YAP_Bool 
mpi_init(void){
  int thread_level;
  char ** my_argv;
  int my_argc = YAP_Argv(&my_argv);
  //  MPI_Init(&GLOBAL_argc, &GLOBAL_argv);
  MPI_Init_thread(&my_argc, &my_argv, MPI_THREAD_MULTIPLE, &thread_level);
#ifdef DEBUG
  write_msg(__FUNCTION__,__FILE__,__LINE__,"Thread level: %d\n",thread_level);
#endif
#ifdef MPISTATS
  RESET_STATS();
#endif
  return  true;
}

#ifdef USE_THREADS
/*
 * Sets up the mpi enviromment. This function should be called before any other MPI
 * function.
 * the argument is the name of the predicate that will be invoked when a message is received
 */
static YAP_Bool 
rcv_msg_thread(char *handle_pred) {
  YAP_Term pred=YAP_MkAtomTerm(YAP_LookupAtom(handle_pred));
  MPI_Status status;

  while(1) { 
    write_msg(__FUNCTION__,__FILE__,__LINE__,"Waiting for MPI msg\n");
    if( MPI_CALL(MPI_Probe( MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status )) == MPI_SUCCESS ) {
      // call handle
      write_msg(__FUNCTION__,__FILE__,__LINE__,"MPI Msg received\n");
      YAP_CallProlog(pred);
    } else
      write_msg(__FUNCTION__,__FILE__,__LINE__,"Error in MPI_Probe\n");
  }
  return 1;
}
/*
 *
 */
static YAP_Bool 
mpi_init_rcv_thread(void){
  int thread_level;
  //  MPI_Init(&GLOBAL_argc, &GLOBAL_argv);
  pthread_t  thread;
  char *arg="handle_msg";

  MPI_Init_thread(&GLOBAL_argc, &GLOBAL_argv,MPI_THREAD_SINGLE,&thread_level);
  if(pthread_create(&thread,NULL,(void*)&rcv_msg_thread,arg)) {
    return  false;
  }

  pthread_detach(thread);
  write_msg(__FUNCTION__,__FILE__,__LINE__,"Thread level: %d\n",thread_level);
  return  true;
}
#endif

/*
 *Terminates the MPI execution enviroment. Every process must call this function before 
 * exiting.
 * mpi_comm_finalize.
 */
static YAP_Bool 
mpi_finalize(void){
  return (MPI_Finalize()==MPI_SUCCESS?true:false);
}
/*
 * Returns the number of workers associated to the MPI_COMM_WORLD communicator.
 * mpi_comm_size(-Size).
 */
static YAP_Bool 
mpi_comm_size(void){
  int	size;
  MPI_CALL(MPI_Comm_size(MPI_COMM_WORLD, &size));
  return (YAP_Unify(YAP_ARG1, YAP_MkIntTerm(size)));
}
/*
 * Returns the rank of the current process.
 * mpi_comm_rank(-Rank).
 */
static YAP_Bool 
mpi_comm_rank(void){
  int  rank;
  MPI_CALL(MPI_Comm_rank(MPI_COMM_WORLD, &rank));
  return(YAP_Unify(YAP_ARG1,YAP_MkIntTerm(rank)));
}
/*
 * Returns the major and minor version of MPI.
 *  mpi_version(-Major,-Minor).
 */
static YAP_Bool 
mpi_version(void){
  int  major,minor;

  MPI_CALL(MPI_Get_version(&major,&minor));
  return (YAP_Unify(YAP_ARG1,YAP_MkIntTerm(major)) && YAP_Unify(YAP_ARG2,YAP_MkIntTerm(minor)));
}
/*
 * 
 * 
 */
static YAP_Bool 
mpi_get_processor_name(void) {
  char name[MPI_MAX_PROCESSOR_NAME];
  int length;
  MPI_CALL(MPI_Get_processor_name(name,&length));
  return (YAP_Unify(YAP_ARG1,YAP_MkAtomTerm(YAP_LookupAtom(name))));
}
/*
 * Non blocking communication function. The message is sent when possible. To check for the status of the message,
 * the mpi_wait and mpi_test should be used. Until mpi_wait is called, the memory allocated for the buffer containing 
 * the message is not released.
 *
 * mpi_isend(+Data, +Destination, +Tag, -Handle).
 */
static YAP_Bool 
mpi_isend(void) {
  YAP_Term t1 = YAP_Deref(YAP_ARG1), 
    t2 = YAP_Deref(YAP_ARG2), 
    t3 = YAP_Deref(YAP_ARG3), 
    t4 = YAP_Deref(YAP_ARG4);
  char *str=NULL;
  int dest,tag;
  size_t len=0;
  MPI_Request *handle=(MPI_Request*)malloc(sizeof(MPI_Request));

  CONT_TIMER();
  if ( handle==NULL ) return  false;

  if (YAP_IsVarTerm(t1) || !YAP_IsIntTerm(t2) || !YAP_IsIntTerm(t3) || !YAP_IsVarTerm(t4)) {
    PAUSE_TIMER();
    return false;
  }
  //
  dest = YAP_IntOfTerm(t2);
  tag  = YAP_IntOfTerm(t3);
  // 
  str=term2string(NULL,&len,t1);
  MSG_SENT(len);
  // send the data 
  if( MPI_CALL(MPI_Isend( str, len, MPI_CHAR, dest, tag, MPI_COMM_WORLD ,handle)) != MPI_SUCCESS ) {
    PAUSE_TIMER();
    return false;
  }

#ifdef DEBUG
  write_msg(__FUNCTION__,__FILE__,__LINE__,"%s(%s,%u, MPI_CHAR,%d,%d)\n",__FUNCTION__,str,len,dest,tag);
#endif
  USED_BUFFER(); //  informs the prologterm2c module that the buffer is now used and should not be messed
  // We must associate the string to each handle
  new_request(handle,str);
  PAUSE_TIMER();
  return(YAP_Unify(YAP_ARG4,YAP_MkIntTerm(HANDLE2INT(handle))));// it should always succeed
}

/*
 * Blocking communication function. The message is sent immediatly.
 * mpi_send(+Data, +Destination, +Tag).
 */
static YAP_Bool 
mpi_send(void) {

  YAP_Term t1 = YAP_Deref(YAP_ARG1), 
    t2 = YAP_Deref(YAP_ARG2), 
    t3 = YAP_Deref(YAP_ARG3);
  char *str=NULL;
  int dest,tag;
  size_t len=0;
  int val;
  if (YAP_IsVarTerm(t1) || !YAP_IsIntTerm(t2) || !YAP_IsIntTerm(t3)) {
    return  false;
  }

  CONT_TIMER();
  //
  dest = YAP_IntOfTerm(t2);
  tag  = YAP_IntOfTerm(t3);
  // the data is packaged as a string
  str=term2string(NULL,&len,t1);
#if  defined(DEBUG) && 0
  write_msg(__FUNCTION__,__FILE__,__LINE__,"%s(%s,%u, MPI_CHAR,%d,%d)\n",__FUNCTION__,str,len,dest,tag);
#endif
  // send the data 
  val=(MPI_CALL(MPI_Send( str, len, MPI_CHAR, dest, tag, MPI_COMM_WORLD))==MPI_SUCCESS?true:false);
  
  PAUSE_TIMER();
  return(val);
}
/*
 * Implements a blocking receive operation. 
 *  mpi_recv(?Source,?Tag,-Data).
 */
static YAP_Bool 
mpi_recv(void) {
  YAP_Term t1 = YAP_Deref(YAP_ARG1), 
    t2 = YAP_Deref(YAP_ARG2), 
    t3 = YAP_Deref(YAP_ARG3), 
    t4;
  int tag, orig;
  int len=0;
  MPI_Status status;

  //The third argument (data) must be unbound
  if(!YAP_IsVarTerm(t3)) {
    return false;
  }
  /* The first argument (Source) must be bound to an integer
     (the rank of the source) or left unbound (i.e. any source
     is OK) */
  if (YAP_IsVarTerm(t1)) orig = MPI_ANY_SOURCE;
  else if( !YAP_IsIntTerm(t1) ) return  false;
  else orig = YAP_IntOfTerm(t1);
  
  /* The second argument must be bound to an integer (the tag)
     or left unbound (i.e. any tag is OK) */
  if (YAP_IsVarTerm(t2)) tag = MPI_ANY_TAG;
  else if( !YAP_IsIntTerm(t2) ) return  false;
  else  tag  = YAP_IntOfTerm( t2 );

  CONT_TIMER();
  // probe for term' size
  if( MPI_CALL(MPI_Probe( orig, tag, MPI_COMM_WORLD, &status )) != MPI_SUCCESS) {
    PAUSE_TIMER();
    return false;
  }
  if( MPI_CALL(MPI_Get_count( &status, MPI_CHAR, &len )) != MPI_SUCCESS || 
      status.MPI_TAG==MPI_UNDEFINED || 
      status.MPI_SOURCE==MPI_UNDEFINED) { 
    PAUSE_TIMER();
    return false;
  }
  //realloc memory buffer
  change_buffer_size((size_t)(len+1));
  BUFFER_LEN=len; 
  // Already know the source from MPI_Probe()
  if( orig == MPI_ANY_SOURCE ) {
    orig = status.MPI_SOURCE;
    if( !YAP_Unify(t1, YAP_MkIntTerm(orig))) {
      PAUSE_TIMER();
      return false;
    }
  }
  // Already know the tag from MPI_Probe()
  if( tag == MPI_ANY_TAG ) {
    tag = status.MPI_TAG;
    if( !YAP_Unify(t2, YAP_MkIntTerm(status.MPI_TAG))) {
      PAUSE_TIMER();
      return false; 
    }
  }
  // Receive the message as a string
  if( MPI_CALL(MPI_Recv( BUFFER_PTR, BUFFER_LEN, MPI_CHAR,  orig, tag,
			 MPI_COMM_WORLD, &status )) != MPI_SUCCESS ) {
    /* Getting in here should never happen; it means that the first
       package (containing size) was sent properly, but there was a glitch with
       the actual content! */
    PAUSE_TIMER();
    return false;
  }
#ifdef DEBUG
  write_msg(__FUNCTION__,__FILE__,__LINE__,"%s(%s,%u, MPI_CHAR,%d,%d)\n",__FUNCTION__,BUFFER_PTR, BUFFER_LEN, orig, tag);
#endif
  MSG_RECV(BUFFER_LEN);
  t4=string2term(BUFFER_PTR,&BUFFER_LEN);
  PAUSE_TIMER();
  return(YAP_Unify(YAP_ARG3,t4));
}

/*
 * Implements a non-blocking receive operation. 
 * mpi_irecv(?Source,?Tag,-Handle).
 */
static YAP_Bool 
mpi_irecv(void) {
  YAP_Term t1 = YAP_Deref(YAP_ARG1), 
    t2 = YAP_Deref(YAP_ARG2), 
    t3 = YAP_Deref(YAP_ARG3);
  int tag, orig;
 MPI_Request *mpi_req=(MPI_Request*)malloc(sizeof(MPI_Request));

   // The third argument (data) must be unbound
  if(!YAP_IsVarTerm(t3)) {
    //Yap_Error(INSTANTIATION_ERROR, t_data, "mpi_receive");
    return false;
  }
  /* The first argument (Source) must be bound to an integer
     (the rank of the source) or left unbound (i.e. any source
     is OK) */
  if (YAP_IsVarTerm(t1)) orig = MPI_ANY_SOURCE;
  else if( !YAP_IsIntTerm(t1) ) return  false;
  else orig = YAP_IntOfTerm(t1);
  
  /* The third argument must be bound to an integer (the tag)
     or left unbound (i.e. any tag is OK) */
  if (YAP_IsVarTerm(t2)) tag = MPI_ANY_TAG;
  else if( !YAP_IsIntTerm(t2) ) return  false;
  else  tag  = YAP_IntOfTerm( t2 );

  CONT_TIMER();
  RESET_BUFFER();
  if(  MPI_CALL(MPI_Irecv( BUFFER_PTR, BLOCK_SIZE, MPI_CHAR, orig, tag,
			   MPI_COMM_WORLD, mpi_req )) != MPI_SUCCESS ) {
    PAUSE_TIMER();
    return false;
  }
  new_request(mpi_req,BUFFER_PTR);
  DEL_BUFFER();
  PAUSE_TIMER();
  return YAP_Unify(t3,YAP_MkIntTerm(HANDLE2INT(mpi_req)));
}

/*
 *  Completes a non-blocking operation. IF the operation was a send, the
 * function waits until the message is buffered or sent by the runtime
 * system. At this point the send buffer is released. If the operation
 * was a receive, it waits until the message is copied to the receive
 * buffer.
 * mpi_wait(+Handle,-Status).
*/
static YAP_Bool 
mpi_wait(void) {
  YAP_Term t1 = YAP_Deref(YAP_ARG1), // Handle
    t2 = YAP_Deref(YAP_ARG2); // Status
  MPI_Status status;
  MPI_Request *handle;

  // The first argument  must be an integer (an handle)
  if(!YAP_IsIntTerm(t1)) {
    return false;
  }
  handle=INT2HANDLE(YAP_IntOfTerm(t1));
  CONT_TIMER();
  // probe for term' size
  if( MPI_CALL(MPI_Wait( handle , &status )) != MPI_SUCCESS ) {
    PAUSE_TIMER();
    return false;
  }
  free_request(handle);
  PAUSE_TIMER();
  return(YAP_Unify(t2,YAP_MkIntTerm(status.MPI_ERROR)));
}

/*
 * mpi_test(+Handle,-Status)
 *
 *  Provides information regarding a handle, ie. if a communication operation has been completed.
 * If the operation has been completed the predicate succeeds with the completion status,
 * otherwise it fails.
 * ).
*/
static YAP_Bool 
mpi_test(void) {
  YAP_Term t1 = YAP_Deref(YAP_ARG1), // Handle
           t2 = YAP_Deref(YAP_ARG2); // Status
  MPI_Status status;
  MPI_Request *handle;
  int flag;

  // The first argument (handle) must be an integer
  if(!YAP_IsIntTerm(t1)) {
    return false;
  }
  CONT_TIMER();

  handle=INT2HANDLE(YAP_IntOfTerm(t1));
  //
  MPI_CALL(MPI_Test( handle , &flag, &status ));
  if( flag != true ) {
    PAUSE_TIMER();
    return false;
  }
  free_request(handle);
  PAUSE_TIMER();
  return(YAP_Unify(t2,YAP_MkIntTerm(status.MPI_ERROR)));
}

/** mpi_wait(+Handle,-Status,-Data
 *   
 *  Completes a non-blocking operation. IF the operation was a send, the
 * function waits until the message is buffered or sent by the runtime
 * system. At this point the send buffer is released. If the operation
 * was a receive, it waits until the message is copied to the receive
 * buffer.
 * .
 */
static YAP_Bool 
mpi_wait_recv(void) {
  YAP_Term t1 = YAP_Deref(YAP_ARG1); // data
  MPI_Status status;
  MPI_Request *handle;
  char *s;
  int len,ret;
  YAP_Term out;

  // The first argument (handle) must be an integer
  if(!YAP_IsIntTerm(t1)) {
    return false;
  }
  CONT_TIMER();

  handle=INT2HANDLE(YAP_IntOfTerm(t1));
  s=(char*)get_request(handle);
  // wait for communication completion
  if( MPI_CALL(MPI_Wait( handle , &status )) != MPI_SUCCESS) {
    PAUSE_TIMER();
    return false;
  }
  len=YAP_SizeOfExportedTerm(s);
  // make sure we only fetch ARG3 after constructing the term
  out = string2term(s,(size_t*)&len);
  MSG_RECV(len);
  free_request(handle);
  PAUSE_TIMER();
  ret=YAP_Unify(YAP_ARG3,out);
  return(ret & YAP_Unify(YAP_ARG2,YAP_MkIntTerm(status.MPI_ERROR)));
}

/*
 * Provides information regarding a handle, ie. if a communication operation has been completed.
 * If the operation has been completed the predicate succeeds with the completion status,
 * otherwise it fails.
 *
 * mpi_test(+Handle,-Status,-Data).
 */
static YAP_Bool 
mpi_test_recv(void) {
  YAP_Term t1 = YAP_Deref(YAP_ARG1); // data

  MPI_Status status;
  MPI_Request *handle;
  int flag,len,ret;
  char *s;
  YAP_Term out;

  // The first argument (handle) must be an integer
  if(!YAP_IsIntTerm(t1)) {
    return false;
  }
  CONT_TIMER();

  handle=INT2HANDLE(YAP_IntOfTerm(t1));
  //
  if( MPI_CALL(MPI_Test( handle , &flag, &status ))!=MPI_SUCCESS) {
    PAUSE_TIMER();
    return false;
  }
  s=(char*)get_request(handle);
  len=strlen(s);
  out = string2term(s,(size_t*)&len);
  // make sure we only fetch ARG3 after constructing the term
  ret=YAP_Unify(YAP_ARG3,out);
  free_request(handle);
  PAUSE_TIMER();
  return(ret & YAP_Unify(YAP_ARG2,YAP_MkIntTerm(status.MPI_ERROR)));
}

/*
 * Collective communication function that performs a barrier synchronization among all processes.
 * mpi_barrier
 */
static YAP_Bool 
mpi_barrier(void) {
  CONT_TIMER();
  int ret=MPI_CALL(MPI_Barrier(MPI_COMM_WORLD));
  PAUSE_TIMER();
  return (ret==MPI_SUCCESS?true:false);
}
/***********************************
 * Broadcast
 ***********************************/
/*
 * Broadcasts a message from the process with rank "root" to
 *      all other processes of the group.
 *  Note: Collective communication means all processes within a communicator call the same routine.
 *       To be able to use a regular MPI_Recv to recv the messages, one should use mpi_bcast2
 *
 *  mpi_bcast(+Root,+Data).
 */
static YAP_Bool 
mpi_bcast(void) {
  YAP_Term t1 = YAP_Deref(YAP_ARG1), 
    t2 = YAP_Deref(YAP_ARG2);
  int root,val;
  size_t len=0;
  char *str;
  int  rank;
  //The arguments should be bound
  if(!YAP_IsIntTerm(t1)) {
    return false;
  }
  MPI_CALL(MPI_Comm_rank(MPI_COMM_WORLD, &rank));

  CONT_TIMER();
  root = YAP_IntOfTerm(t1);
  if (root == rank) {
    str=term2string(NULL,&len,t2);
#ifdef DEBUG
    write_msg(__FUNCTION__,__FILE__,__LINE__,"mpi_bcast(%s,%u, MPI_CHAR,%d)\n",str,len,root);
#endif
  } else {
    RESET_BUFFER();
    str = BUFFER_PTR;
    len = BLOCK_SIZE;
  }
  // send the data 
  val=(MPI_CALL(MPI_Bcast( str, len, MPI_CHAR, root, MPI_COMM_WORLD))==MPI_SUCCESS?true:false);


#ifdef MPISTATS
  {
   int	size;
   MPI_CALL(MPI_Comm_size(MPI_COMM_WORLD, &size));
   MSG_SENT(len*size);
  }
#endif
  PAUSE_TIMER();
  if (root != rank) {
    YAP_Term out;
    len=YAP_SizeOfExportedTerm(str);
    // make sure we only fetch ARG3 after constructing the term
    out = string2term(str,(size_t*)&len);
    MSG_RECV(len);
    if (!YAP_Unify(YAP_ARG2, out))
      return false;
  }
  return(val);
}

/*
 * Broadcasts a message from the process with rank "root" to
 *      all other processes of the group.
 * Note: Collective communication means all processes within a communicator call the same routine.
 *       To be able to use a regular MPI_Recv to recv the messages, one should use mpi_bcast2
 * mpi_bcast_int(+Root,+Data,+Tag).
 */
static YAP_Bool 
my_bcast(YAP_Term t1,YAP_Term t2, YAP_Term t3) {
  int root;
  int k,worldsize;
  size_t len=0;
  char *str;
  int tag;

  //The arguments should be bound
  if(YAP_IsVarTerm(t2) || !YAP_IsIntTerm(t1) || !YAP_IsIntTerm(t3)) {
    return false;
  }
  
  CONT_TIMER();

  MPI_CALL(MPI_Comm_size(MPI_COMM_WORLD,&worldsize));

  root = YAP_IntOfTerm(t1);
  tag = YAP_IntOfTerm(t3);
  str=term2string(NULL,&len,t2);
  
  for(k=0;k<=worldsize-1;++k)
    if(k!=root) {
      // Use async send?
      MSG_SENT(len);
      if(MPI_CALL(MPI_Send( str, len, MPI_CHAR, k, tag, MPI_COMM_WORLD))!=MPI_SUCCESS) {
	PAUSE_TIMER();
	return false;
      }
#ifdef DEBUG
  write_msg(__FUNCTION__,__FILE__,__LINE__,"bcast2(%s,%u, MPI_CHAR,%d,%d)\n",str,len,k,tag);
#endif
    }
  PAUSE_TIMER();
  return true;
}
/*
 * mpi_bcast(+Root,+Data).
 */
static YAP_Bool 
mpi_bcast2(void) {
  return my_bcast(YAP_ARG1,YAP_ARG2,YAP_MkIntTerm(0));
}
/*
 * Broadcasts a message from the process with rank "root" to
 *      all other processes of the group.
 * Note: Collective communication means all processes within a communicator call the same routine.
 *       To be able to use a regular MPI_Recv to recv the messages, one should use mpi_bcast2
 *
 * mpi_bcast(+Root,+Data,+Tag).
 */
static YAP_Bool 
mpi_bcast3(void) {
  return my_bcast(YAP_ARG1,YAP_ARG2,YAP_ARG3);
}
/*
 * Broadcasts a message from the process with rank "root" to
 *      all other processes of the group.
 * mpi_ibcast(+Root,+Data,+Tag).
 */
static YAP_Bool 
my_ibcast(YAP_Term t1,YAP_Term t2, YAP_Term t3) {
  int root;
  int k,worldsize;
  size_t len=0;
  char *str;
  int tag;
  BroadcastRequest *b;

  //fprintf(stderr,"ibcast1");
  //The arguments should be bound
  if(YAP_IsVarTerm(t2) || !YAP_IsIntTerm(t1) || !YAP_IsIntTerm(t3)) {
    return false;
  }

  CONT_TIMER();

  //  fprintf(stderr,"ibcast2");
  MPI_CALL(MPI_Comm_size(MPI_COMM_WORLD,&worldsize));

  root = YAP_IntOfTerm(t1);
  tag = YAP_IntOfTerm(t3);
  str = term2string(NULL,&len,t2);
  b=new_broadcast();
  if ( b==NULL ) {
    PAUSE_TIMER();
    return false;
  }
  //fprintf(stderr,"ibcast3");
  for(k=0;k<=worldsize-1;++k) {
    if(k!=root) {
      MPI_Request *handle=(MPI_Request*)malloc(sizeof(MPI_Request));
      MSG_SENT(len);
      // Use async send
      if(MPI_CALL(MPI_Isend(str, len, MPI_CHAR, k, tag, MPI_COMM_WORLD,handle))!=MPI_SUCCESS) {
	free(handle);
	PAUSE_TIMER();
	return false;
      }
      new_broadcast_request(b,handle,str);
      //new_request(handle,str);
      USED_BUFFER();
    }
  }
  if(!b->nreq)//release b if no messages were sent (worldsize==1)
    free(b);

#if defined(DEBUG) && defined(MALLINFO)
  {
    struct mallinfo s = mallinfo();
    printf("%d: %d=%d/%d\n",getpid(),s.arena,s.uordblks,s.fordblks); //vsc
  }
#endif
  PAUSE_TIMER();
  //fprintf(stderr,"ibcast4");
  return true;
}
/*
 * Broadcasts a message from the process with rank "root" to
 *      all other processes of the group.
 * To receive the message the recipients use MPI_Recv
 * The message is sent using MPI_Isend
 * mpi_ibcast(+Root,+Data,+Tag).
 */
static YAP_Bool 
mpi_ibcast3(void) {
  return my_ibcast(YAP_ARG1,YAP_ARG2,YAP_ARG3);
}
/*
 * mpi_ibcast(+Root,+Data).
 */
static YAP_Bool 
mpi_ibcast2(void) {
  return my_ibcast(YAP_ARG1,YAP_ARG2,YAP_MkIntTerm(0));
}
/*******************************************
 * Garbage collection
 *******************************************/
/*
 * Attempts to release the requests structures used in asynchronous communications
 */
static void 
gc(hashtable ht) {
  MPI_Request *handle;
  hashnode* node;
  MPI_Status status;
  int flag;

  node=(hashnode*)next_hashnode(ht);
  if ( node==NULL) return;
  
  gc(ht); // start at the end
  
  handle=INT2HANDLE(node->value);
  MPI_CALL(MPI_Test( handle , &flag, &status ));
  if ( flag==true) {
    MPI_CALL(MPI_Wait(handle,&status));
#ifdef DEBUG
    write_msg(__FUNCTION__,__FILE__,__LINE__,"Released handle...%s\n",(char*)node->obj);
#endif
    if (ht==requests)
      free_request(handle);
    else
      free_broadcast_request(handle);
  }
}
/*
 * 
 */
static YAP_Bool 
mpi_gc(void) {
  //write_msg(__FUNCTION__,__FILE__,__LINE__,"MPI_gc>: requests=%d\n",requests->n_entries);
  CONT_TIMER();
  init_hash_traversal(requests);
  gc(requests);
  init_hash_traversal(broadcasts);
  gc(broadcasts);
  //write_msg(__FUNCTION__,__FILE__,__LINE__,"MPI_gc<: requests=%d\n",requests->n_entries);
  PAUSE_TIMER();
  return true;
}

size_t BLOCK_SIZE=4*1024;

static YAP_Bool
mpi_default_buffer_size(void)
{
  YAP_Term t2;
  intptr_t IBLOCK_SIZE;
  if (!YAP_Unify(YAP_ARG1,YAP_MkIntTerm(BLOCK_SIZE)))
    return false;
  t2 = YAP_ARG2;
  if (YAP_IsVarTerm(t2))
    return true;
  if (!YAP_IsIntTerm(t2))
    return false;
  IBLOCK_SIZE= YAP_IntOfTerm(t2);
  if (IBLOCK_SIZE < 0) {
    IBLOCK_SIZE=4*1024;
    return false;
  }
  BLOCK_SIZE = IBLOCK_SIZE;
  return true;
}

/********************************************************************
 * Init
 *******************************************************************/
X_API void 
init_mpi(void) {

  requests=new_hashtable(HASHSIZE);
  broadcasts=new_hashtable(HASHSIZE);
  DEL_BUFFER();
  YAP_UserCPredicate( "mpi_init",  mpi_init,0);                            // mpi_init/0
#ifdef USE_THREADS
  YAP_UserCPredicate( "mpi_init_rcv_thread", mpi_init_rcv_thread,1);       // mpi_init_rcv_thread(+HandleMsgGoal/1)
#endif
  YAP_UserCPredicate( "mpi_finalize", mpi_finalize,0);                     // mpi_finalize turn
  YAP_UserCPredicate( "mpi_comm_size", mpi_comm_size,1);                   // mpi_comm_size(-Size)
  YAP_UserCPredicate( "mpi_comm_rank",  mpi_comm_rank,1);                  // mpi_comm_rank(-Rank)
  YAP_UserCPredicate( "mpi_version", mpi_version,2);                       // mpi_version(-Major,-Minor)
  YAP_UserCPredicate( "mpi_get_processor_name", mpi_get_processor_name,1); // mpi_get_processor_name(-Name)
  YAP_UserCPredicate( "mpi_send", mpi_send,3);                             // mpi_send(+Data, +Destination, +Tag).
  YAP_UserCPredicate( "mpi_isend",mpi_isend,4);
  YAP_UserCPredicate( "mpi_recv", mpi_recv,3);                             // mpi_recv(?Source,?Tag,-Data).
  YAP_UserCPredicate( "mpi_irecv", mpi_irecv,3);                           // mpi_irecv(?Source,?Tag,-Handle).
  YAP_UserCPredicate( "mpi_wait", mpi_wait,2);                             // mpi_wait(+Handle,-Status).
  YAP_UserCPredicate( "mpi_wait_recv", mpi_wait_recv,3);                    // mpi_wait_recv(+Handle,-Status,-Data).
  YAP_UserCPredicate( "mpi_test", mpi_test,2);                             // mpi_test(+Handle,-Status).
  YAP_UserCPredicate( "mpi_test_recv", mpi_test_recv,3);                    // mpi_test(+Handle,-Status,-Data).
  YAP_UserCPredicate( "mpi_bcast", mpi_bcast,2);                           // mpi_bcast(Root,Term)
  YAP_UserCPredicate( "mpi_bcast2", mpi_bcast2,2);                         // mpi_bcast2(Root,Term)
  YAP_UserCPredicate( "mpi_bcast3", mpi_bcast3,3);                         // mpi_bcast3(Root,Term,Tag)
/** @pred mpi_bcast3(+ _Root_, + _Data_, + _Tag_)


Broadcasts the message  _Data_ with tag  _Tag_ from the process with rank  _Root_
to all other processes.

 
*/
  YAP_UserCPredicate( "mpi_ibcast", mpi_ibcast2,2);                         // mpi_ibcast(Root,Term)
  YAP_UserCPredicate( "mpi_ibcast", mpi_ibcast3,3);                         // mpi_ibcast(Root,Term,Tag)
/** @pred mpi_ibcast(+ _Root_, + _Data_, + _Tag_) 



Non-blocking operation. Broadcasts the message  _Data_ with tag  _Tag_
from the process with rank  _Root_ to all other processes.

 
*/
  YAP_UserCPredicate( "mpi_barrier", mpi_barrier,0);                       // mpi_barrier/0
  YAP_UserCPredicate( "mpi_gc", mpi_gc,0);                                 // mpi_gc/0
  YAP_UserCPredicate( "mpi_default_buffer_size", mpi_default_buffer_size,2);        // buffer size
/** @pred mpi_default_buffer_size(- _OldBufferSize_, ? _NewBufferSize_) 



The  _OldBufferSize_ argument unifies with the current size of the
MPI communication buffer size and sets the communication buffer size
 _NewBufferSize_. The buffer is used for assynchronous waiting and
for broadcast receivers. Notice that buffer is local at each MPI
process.

 
*/
#ifdef MPISTATS
  YAP_UserCPredicate( "mpi_stats", mpi_stats,7);                            // mpi_stats(-Time,#MsgsRecv,BytesRecv,MaxRecev,#MsgSent,BytesSent,MaxSent)
  YAP_UserCPredicate( "mpi_reset_stats", mpi_reset_stats,0);                // cleans the timers
  RESET_STATS();
#endif
  //  YAP_UserCPredicate( "mpi_gather", mpi_gather,0); //mpi_gather(+RootRank,?SendData,?RecvData)
  // Each process (root process included) sends the contents of its send buffer to the root process. The root process receives the messages and stores them in rank order. The outcome is  as if each of the  n processes in the group (including the root process) had executed a call to MPI_Send and the root had executed n calls to MPI_Recv.  The receive buffer is ignored for all non-root processes.
  // MPI_Scatter
#ifdef DEBUG
  fprintf(stderr,"MPI  module succesfully loaded.");
  fflush(stderr);
#endif
}

#endif /* HAVE_MPI_H */
