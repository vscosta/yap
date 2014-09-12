
@defgroup Threads Threads
@ingroup YAPExtensions
@{

YAP implements a SWI-Prolog compatible multithreading
library. Like in SWI-Prolog, Prolog threads have their own stacks and
only share the Prolog <em>heap</em>: predicates, records, flags and other
global non-backtrackable data.  The package is based on the POSIX thread
standard (Butenhof:1997:PPT) used on most popular systems except
for MS-Windows.

@defgroup Creating_and_Destroying_Prolog_Threads Creating and Destroying Prolog Threads
@ingroup Threads

@pred thread_create(: _Goal_, - _Id_, + _Options_)

Create a new Prolog thread (and underlying C-thread) and start it
by executing  _Goal_.  If the thread is created successfully, the
thread-identifier of the created thread is unified to  _Id_.
 _Options_ is a list of options.  Currently defined options are:

+ stack
Set the limit in K-Bytes to which the Prolog stacks of
this thread may grow.  If omitted, the limit of the calling thread is
used.  See also the  commandline `-S` option.

+ trail
Set the limit in K-Bytes to which the trail stack of this thread may
grow.  If omitted, the limit of the calling thread is used. See also the
commandline option `-T`.

+ alias
Associate an alias-name with the thread.  This named may be used to
refer to the thread and remains valid until the thread is joined
(see thread_join/2).

+ at_exit
Define an exit hook for the thread.  This hook is called when the thread
terminates, no matter its exit status.

+ detached
If `false` (default), the thread can be waited for using
thread_join/2. thread_join/2 must be called on this thread
to reclaim the all resources associated to the thread. If `true`,
the system will reclaim all associated resources automatically after the
thread finishes. Please note that thread identifiers are freed for reuse
after a detached thread finishes or a normal thread has been joined.
See also thread_join/2 and thread_detach/1.


The  _Goal_ argument is <em>copied</em> to the new Prolog engine.
This implies further instantiation of this term in either thread does
not have consequences for the other thread: Prolog threads do not share
data from their stacks.

 @defgroup Monitoring_Threads Monitoring Threads
@ingroup Threads


Normal multi-threaded applications should not need these the predicates
from this section because almost any usage of these predicates is
unsafe. For example checking the existence of a thread before signalling
it is of no use as it may vanish between the two calls. Catching
exceptions using catch/3 is the only safe way to deal with
thread-existence errors.

These predicates are provided for diagnosis and monitoring tasks.

 



 @defgroup Thread_Communication Thread communication
@ingroup Threads


Prolog threads can exchange data using dynamic predicates, database
records, and other globally shared data. These provide no suitable means
to wait for data or a condition as they can only be checked in an
expensive polling loop. <em>Message queues</em> provide a means for
threads to wait for data or conditions without using the CPU.

Each thread has a message-queue attached to it that is identified
by the thread. Additional queues are created using
`message_queue_create/2`.



 @pred thread_send_message(+ _Term_) 


Places  _Term_ in the message-queue of the thread running the goal. 
Any term can be placed in a message queue, but note that the term is 
copied to the receiving thread and variable-bindings are thus lost. 
This call returns immediately.

 



 @defgroup Signalling_Threads Signalling Threads
@ingroup Threadas


These predicates provide a mechanism to make another thread execute some
goal as an <em>interrupt</em>.  Signalling threads is safe as these
interrupts are only checked at safe points in the virtual machine.
Nevertheless, signalling in multi-threaded environments should be
handled with care as the receiving thread may hold a <em>mutex</em>
(see with_mutex/2).  Signalling probably only makes sense to start
debugging threads and to cancel no-longer-needed threads with throw/1,
where the receiving thread should be designed carefully do handle
exceptions at any point.

 



 @defgroup Threads_and_Dynamic_Predicates Threads and Dynamic Predicates
@ingroup Threads


Besides queues threads can share and exchange data using dynamic
predicates. The multi-threaded version knows about two types of
dynamic predicates. By default, a predicate declared <em>dynamic</em>
(see dynamic/1) is shared by all threads. Each thread may
assert, retract and run the dynamic predicate. Synchronisation inside
Prolog guarantees the consistency of the predicate. Updates are
<em>logical</em>: visible clauses are not affected by assert/retract
after a query started on the predicate. In many cases primitive from
thread synchronisation should be used to ensure application invariants on
the predicate are maintained.

Besides shared predicates, dynamic predicates can be declared with the
thread_local/1 directive. Such predicates share their
attributes, but the clause-list is different in each thread.

 



@defgroup Thread_Synchronisation Thread Synchronisation
@ingroup Threads

All
 internal Prolog operations are thread-safe. This implies two Prolog
threads can operate on the same dynamic predicate without corrupting the
consistency of the predicate. This section deals with user-level
<em>mutexes</em> (called <em>monitors</em> in ADA or
<em>critical-sections</em> by Microsoft).  A mutex is a
<em>MUT</em>ual <em>EX</em>clusive device, which implies at most one thread
can <em>hold</em> a mutex.

Mutexes are used to realise related updates to the Prolog database.
With `related', we refer to the situation where a `transaction' implies
two or more changes to the Prolog database.  For example, we have a
predicate `address/2`, representing the address of a person and we want
to change the address by retracting the old and asserting the new
address.  Between these two operations the database is invalid: this
person has either no address or two addresses, depending on the
assert/retract order.

Here is how to realise a correct update:

~~~~~
:- initialization
    mutex_create(addressbook).

change_address(Id, Address) :-
    mutex_lock(addressbook),
    retractall(address(Id, _)),
    asserta(address(Id, Address)),
    mutex_unlock(addressbook).
~~~~~

 

@}
