% Author: Nuno A. Fonseca
% Date: 2006-06-01
% $Id: lam_mpi.yap,v 1.1 2006-06-04 18:43:38 nunofonseca Exp $


:- module(lam_mpi, [
                  mpi_init/0,
                  mpi_finalize/0,
                  mpi_comm_size/1,
                  mpi_comm_rank/1,
                  mpi_version/2,
		  mpi_send/3,
		  mpi_isend/4,
		  mpi_recv/3,
		  mpi_irecv/3,
		  mpi_wait/2,
		  mpi_wait_recv/3,
		  mpi_test/2,
		  mpi_test_recv/3,
		  mpi_bcast/2,
		  mpi_ibcast2/2,
		  mpi_ibcast2/3,
		  mpi_bcast2/2,
		  mpi_bcast2/3,
		  mpi_barrier/0,
		  mpi_msg_buffer_size/2,
		  mpi_msg_size/2,
		  mpi_gc/0,
		  mpi_default_buffer_size/2
                   ]).

/**
 * @defgroup lam_mpi MPI Interface
 * @ingroup library
@{

This library provides a set of utilities for interfacing with LAM MPI.
The following routines are available once included with the
`use_module(library(lam_mpi))` command. The yap should be
invoked using the LAM mpiexec or mpirun commands (see LAM manual for
more details).

 
*/


/** @pred mpi_barrier 


Collective communication predicate.  Performs a barrier
synchronization among all processes. Note that a collective
communication means that all processes call the same predicate. To be
able to use a regular `mpi_recv` to receive the messages, one
should use `mpi_bcast2`.
*/
/** @pred mpi_bcast2(+ _Root_, ? _Data_) 



Broadcasts the message  _Data_ from the process with rank  _Root_
to all other processes.

 
*/
/** @pred mpi_comm_rank(- _Rank_) 


Unifies  _Rank_ with the rank of the current process in the MPI environment.

 
*/
/** @pred mpi_comm_size(- _Size_) 


Unifies  _Size_ with the number of processes in the MPI environment.

 
*/
/** @pred mpi_finalize 


Terminates the MPI execution environment. Every process must call this predicate before  exiting.

 
*/
/** @pred mpi_gc 



Attempts to perform garbage collection with all the open handles
associated with send and non-blocking broadcasts. For each handle it
tests it and the message has been delivered the handle and the buffer
are released.




 */
/** @pred mpi_init 


Sets up the mpi environment. This predicate should be called before any other MPI predicate.

 
*/
/** @pred mpi_irecv(? _Source_,? _Tag_,- _Handle_) 



Non-blocking communication predicate. The predicate returns an
 _Handle_ for a message that will be received from processor with
rank  _Source_ and tag  _Tag_. Note that the predicate succeeds
immediately, even if no message has been received. The predicate
`mpi_wait_recv` should be used to obtain the data associated to
the handle.

 
*/
/** @pred mpi_isend(+ _Data_,+ _Dest_,+ _Tag_,- _Handle_) 



Non blocking communication predicate. The message in  _Data_, with
tag  _Tag_, is sent whenever possible to the processor with rank
 _Dest_. An  _Handle_ to the message is returned to be used to
check for the status of the message, using the `mpi_wait` or
`mpi_test` predicates. Until `mpi_wait` is called, the
memory allocated for the buffer containing the message is not
released.

 
*/
/** @pred mpi_msg_size( _Msg_, - _MsgSize_) 


Unify  _MsgSize_ with the number of bytes YAP would need to send the
message  _Msg_.

 
*/
/** @pred mpi_recv(? _Source_,? _Tag_,- _Data_) 



Blocking communication predicate. The predicate blocks until a message
is received from processor with rank  _Source_ and tag  _Tag_.
The message is placed in  _Data_.

 
*/
/** @pred mpi_send(+ _Data_,+ _Dest_,+ _Tag_) 



Blocking communication predicate. The message in  _Data_, with tag
 _Tag_, is sent immediately to the processor with rank  _Dest_.
The predicate succeeds after the message being sent.

 
*/
/** @pred mpi_test(? _Handle_,- _Status_) 



Provides information regarding the handle  _Handle_, ie., if a
communication operation has been completed.  If the operation
associate with  _Hanlde_ has been completed the predicate succeeds
with the completion status in  _Status_, otherwise it fails.

 
*/
/** @pred mpi_test_recv(? _Handle_,- _Status_,- _Data_) 



Provides information regarding a handle. If the message associated
with handle  _Hanlde_ is buffered then the predicate succeeds
unifying  _Status_ with the status of the message and  _Data_
with the message itself. Otherwise, the predicate fails.

 
*/
/** @pred mpi_version(- _Major_,- _Minor_) 


Unifies  _Major_ and  _Minor_ with, respectively, the major and minor version of the MPI.

 
*/
/** @pred mpi_wait(? _Handle_,- _Status_) 



Completes a non-blocking operation. If the operation was a
`mpi_send`, the predicate blocks until the message is buffered
or sent by the runtime system. At this point the send buffer is
released. If the operation was a `mpi_recv`, it waits until the
message is copied to the receive buffer.  _Status_ is unified with
the status of the message.

 
*/
/** @pred mpi_wait_recv(? _Handle_,- _Status_,- _Data_) 



Completes a non-blocking receive operation. The predicate blocks until
a message associated with handle  _Hanlde_ is buffered. The
predicate succeeds unifying  _Status_ with the status of the
message and  _Data_ with the message itself. 

 
*/

:- load_foreign_files([yap_mpi], [], init_mpi).

mpi_msg_size(Term, Size) :-
	terms:export_term(Term, Buf, Size),
	terms:kill_exported_term(Buf).
/** @} */

