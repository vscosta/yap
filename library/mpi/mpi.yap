	% Author: Nuno A. Fonseca
% Date: 2006-06-01
% $Id: lam_mpi.yap,v 1.1 2006-06-04 18:43:38 nunofonseca Exp $


:- module(mpi, [
                  mpi_init/0,
		  mpi_stop/0,
                  mpi_finalize/0,
                  mpi_comm_size/1,
                  mpi_comm_rank/1,
                  mpi_version/2,
		  mpi_send/3,
		  mpi_isend/4,
		  mpi_recv/3,
		  mpi_irecv/4,
		  mpi_wait/1,
		  mpi_wait/2,
		  mpi_test/1,
		  mpi_test/2,
		  mpi_bcast/2,
		  mpi_bcast/3,
		  mpi_ibcast/3,
		  mpi_ibcast/4,
		  mpi_barrier/0,
		  mpi_buffer_size/2,
		  mpi_default_buffer_size/1
                   ]).

/**
 * @defgroup  MPI Interface
 * @ingroup YAPLibrary
@{

This library provides a set of utilities for interfacing with MPI.
The following routines are available once included with the
`use_module(library(lam_mpi))` command. The yap binaryshould be
invoked using the  mpiexec or mpirun commands (see OPENMPI or MPICH manual for
more details).

The following features are available:

- synchroneous send, broadcast, and receive. Term size is managed by the library
- asynchroneous send and broadcast: these operations must be completed by an mpi_test ot mpi_wait.
- asynchroneous receive: size of the data buffer must be provided to the library; data is available at a test or wait call.
- intialization, barriers, and finalization.

Note that synchroneous and asynchroneous send receive can be combined, eg: async send and symc receive.

The examples directory shows examples of these techniques.

*/

:- load_foreign_files([], [YAPmpi], init_mpi).

mpi_bcast(Root,Data) :-
    mpi_bcast(Root,_,Data).

mpi_ibcast(Root,Data,Req) :-
    mpi_ibcast(Root,_,Data, Req).

mpi_wait_recv(Root,Data,Req) :-
    mpi_wait(Root,Data, Req).


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
/**


Sets up the mpi environment. This predicate should be called before any other MPI predicate.


*/
/** @pred mpi_irecv(? _Source_,? _Tag_,+Size,- _Handle_)



Non-blocking communication predicate. The predicate returns an
 _Handle_ for a message that will be received from processor with
rank  _Source_ and tag  _Tag_. Note that the predicate succeeds
immediately, even if no message has been received. The predicate
`mpi_wait_recv` should be used to obtain the data associated to
the handle. The term must fit in _Size_ bytes.


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
/** @pred mpi_version(- _Major_,- _Minor_)


Unifies  _Major_ and  _Minor_ with, respectively, the major and minor version of the MPI.


*/
/** @pred mpi_wait_recv(? _Handle_,- _Status_,- _Data_)



Completes a non-blocking receive operation. The predicate blocks until
a message associated with handle  _Hanlde_ is buffered. The
predicate succeeds unifying  _Status_ with the status of the
message and  _Data_ with the message itself.


*/

mpi_msg_size(Term, Size) :-
	terms:export_term(Term, Buf, Size),
	terms:kill_exported_term(Buf).
/** @} */
