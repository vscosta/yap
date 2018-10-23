/**
  * @fileChunkSize library/sockets.yap
*/

:- module(yap_sockets,
	  [ ip_socket/2,		% +Domain, -Socket
	    ip_socket/4,		% +Domain, +Type, +Protocol, -Socket
	    socket_close/1,		% +Socket
	    socket_bind/2,		% +Socket, 'AF_INET'(+Host,+Port)
	    tcp_socket_connect/3,	% +Socket, 'AF_INET'(+Host,+Port), -Stream
	    socket_listen/2,		% +Socket, +Length
	    socket_accept/2,		% +Socket, -Stream
	    socket_accept/3,		% +Socket, -Client, -Stream
%	    socket_select/5,		% +TermsSockets, -NewTermsStreams,
	    				% +TimeOut, +Streams, -ReadStreams
	    current_host/1,		% ?HostName
	    hostname_address/2		% ?HostName, ?HostAddress
	  ]).
:- use_module(library(socket)).
:- use_module(library(error)).
:- use_module(library(apply)).

/** uses SWI code

@aaddtogroup SICStus compatible socket library

@ingroup builtins

YAP includes a SICStus Prolog compatible socket interface. In YAP-6.3
this uses the `clib` package to emulate the old low level interface that
provides direct access to the major socket system calls. These calls
can be used both to open a new connection in the network or connect to
a networked server. Socket connections are described as read/write
streams, and standard Input/Output built-ins can be used to write on or read
from sockets. The following calls are available:


@tbd Our implementation does not support AF_UNIX sockets.
@tbd Implement socket_select/5
@see http://www.sics.se/sicstus/docs/3.7.1/html/sicstus_28.html
*/


/** @pred  current_host(? _HOSTNAME_) 

Unify  _HOSTNAME_ with an atom representing the fully qualified
hostname for the current host. Also succeeds if  _HOSTNAME_ is bound
to the unqualified hostname.

 
*/
/** @pred  hostname_address(? _HOSTNAME_,? _IP_ADDRESS_) 

 _HOSTNAME_ is an host name and  _IP_ADDRESS_ its IP
address in number and dots notation.




*/
/** @pred  socket_accept(+ _SOCKET_, - _CLIENT_, - _STREAM_) 


Interface to system call `accept`, used for servers to wait for
connections at socket  _SOCKET_. The stream descriptor  _STREAM_
represents the resulting connection.  If the socket belongs to the
domain `AF_INET`,  _CLIENT_ unifies with an atom containing
the IP address for the client in numbers and dots notation.

 
*/
/** @pred  socket_accept(+ _SOCKET_, - _STREAM_)

Accept a connection but do not return client information.

 
*/
/** @pred  socket_bind(+ _SOCKET_, ? _PORT_) 



Interface to system call `bind`, as used for servers: bind socket
to a port. Port information depends on the domain:

+ 'AF_UNIX'(+ _FILENAME_) (unsupported)
+ 'AF_FILE'(+ _FILENAME_)
use file name  _FILENAME_ for UNIX or local sockets.

+ 'AF_INET'(? _HOST_,?PORT)
If  _HOST_ is bound to an atom, bind to host  _HOST_, otherwise
if unbound bind to local host ( _HOST_ remains unbound). If port
 _PORT_ is bound to an integer, try to bind to the corresponding
port. If variable  _PORT_ is unbound allow operating systems to
choose a port number, which is unified with  _PORT_.



 
*/
/** @pred  socket_close(+ _SOCKET_) 



Close socket  _SOCKET_. Note that sockets used in
`socket_connect` (that is, client sockets) should not be closed with
`socket_close`, as they will be automatically closed when the
corresponding stream is closed with close/1 or `close/2`.

 
*/
/** @pred  socket_listen(+ _SOCKET_, + _LENGTH_) 


Interface to system call `listen`, used for servers to indicate
willingness to wait for connections at socket  _SOCKET_. The
integer  _LENGTH_ gives the queue limit for incoming connections,
and should be limited to `5` for portable applications. The socket
must be of type `SOCK_STREAM` or `SOCK_SEQPACKET`.

 
*/

%socket(+@var{DOMAIN},+@var{TYPE},+@var{PROTOCOL},-@var{SOCKET})

ip_socket(Domain, 'SOCK_DGRAM', Protocol, SOCKET) :-
    must_be(oneof(['AF_INET']), Domain),
    must_be(oneof([0]), Protocol),
    udp_socket(SOCKET),
    assert(yap_socket(udp, SOCKET)).
ip_socket(Domain, 'SOCK_STREAM', Protocol, SOCKET)  :-
    must_be(oneof(['AF_INET']), Domain),
    must_be(oneof([0]), Protocol),
    tcp_socket(SOCKET),
    assert(yap_socket(tcp, SOCKET)).

ip_socket(Domain, SOCK) :-
    socket(Domain, 'SOCK_STREAM', 0, SOCK).

socket_close(Socket) :-
    retract(yap_socket(udp, Socket)), !.
socket_close(Socket) :-
    retract(yap_socket(tcp, Socket)), !,
    tcp_close_socket(Socket).

socket_bind(Socket, 'AF_INET'(Host,Port)) :-
	(   Address = 'AF_INET'(Host, Port)
	->  true
	;   type_error(socket_address, Address)
	),
	(   var(Host)
	->  gethostname(Host)
	;   true			% Warning?
	),
	tcp_bind(Socket, Port).

tcp_socket_connect(Socket, Address, StreamPair) :-
    (   Address = 'AF_INET'(Host, Port)
    ->  true
    ;   type_error(socket_address, Address)
    ),
    tcp_connect(Socket, Host:Port),
    tcp_open_socket(Socket, Read, Write),
    stream_pair(StreamPair, Read, Write).

socket_listen(SOCKET, BACKLOG) :-
    tcp_listen(SOCKET, BACKLOG).

socket_accept(Socket, Client, StreamPair) :-
    tcp_accept(Socket, Socket2, Peer),
    peer_to_client(Peer, Client),
    tcp_open_socket(Socket2, Read, Write),
    stream_pair(StreamPair, Read, Write).

/** @pred  socket_buffering(+ _SOCKET_, - _MODE_, - _OLD_, + _NEW_) 


Set buffering for  _SOCKET_ in `read` or `write`
 _MODE_.  _OLD_ is unified with the previous status, and  _NEW_
receives the new status which may be one of `unbuf` or
`fullbuf`.

 
*/
socket_buffering(STREAM, _, CUR, NEW) :-
    stream_property(STREAM, buffer(Prop) ),
    translate_buffer(Prop, CUR),
    translate_buffer(NProp, NEW),
    stream_property(STREAM, buffer(NProp) ).

translate_buffer(false, unbuf).
translate_buffer(full, fullbuf).

current_host(Host) :-
    gethostname(Host).

hostname_address(Host, Address) :-
    nonvar(Host), !,
    tcp_host_to_address(Host, IP),
    peer_to_client(IP, Address).

peer_to_client(ip(A,B,C,D), Client) :-
    Parts = [A,B,C,D],
    ground(Parts), !,
    atomic_list_concat(Parts, '.', Client).
peer_to_client(ip(A,B,C,D), Client) :-
    atomic_list_concat(Parts, '.', Client),
    maplist(atom_number, Parts, Numbers),
    Numbers = [A,B,C,D].

/** @pred  socket_select(+ _SOCKETS_, - _NEWSTREAMS_, + _TIMEOUT_, + _STREAMS_, - _READSTREAMS_) [unsupported in YAP-6.3]

Interface to system call `select`, used for servers to wait for
connection requests or for data at sockets. The variable
 _SOCKETS_ is a list of form  _KEY-SOCKET_, where  _KEY_ is
an user-defined identifier and  _SOCKET_ is a socket descriptor. The
variable  _TIMEOUT_ is either `off`, indicating execution will
wait until something is available, or of the form  _SEC-USEC_, where
 _SEC_ and  _USEC_ give the seconds and microseconds before
socket_select/5 returns. The variable  _SOCKETS_ is a list of
form  _KEY-STREAM_, where  _KEY_ is an user-defined identifier
and  _STREAM_ is a stream descriptor

Execution of socket_select/5 unifies  _READSTREAMS_ from
 _STREAMS_ with readable data, and  _NEWSTREAMS_ with a list of
the form  _KEY-STREAM_, where  _KEY_ was the key for a socket
with pending data, and  _STREAM_ the stream descriptor resulting
from accepting the connection.  

 
*/
socket_select(_,_,_,_,_) :-
    format( user_error, "Unsupported in this version, please use wait_for_input/3~n", []).

/**
@}
*/
