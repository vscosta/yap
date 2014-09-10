/** uses SWI code

<module> SICStus compatible socket library

@tbd Our implementation does not support AF_UNIX sockets.
@TBD Implement socket_select/5
@see http://www.sics.se/sicstus/docs/3.7.1/html/sicstus_28.html
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

socket_select(_,_,_,_,_) :-
    format( user_error, "Unsupported in this version, please use wait_for_input/3~n", []).
