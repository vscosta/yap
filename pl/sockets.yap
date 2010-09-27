/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		sockets.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	Socket predicates			 		 *
*									 *
*************************************************************************/

socket(S,D) :- 
	socket(S, 'SOCK_STREAM', 0, D).

socket_accept(S,F) :- 
	socket_accept(S, _, F).

socket_select(Socks, OutSocks, TimeOut, Streams, OutStreams) :-
	'$check_list_for_sockets'(Socks, socket_select(Socks, OutSocks, TimeOut, Streams, OutStreams)),
	'$check_list_for_sockets'(Streams, socket_select(Socks, OutSocks, TimeOut, Streams, OutStreams)),
	'$select_cp_fds'(Socks, Streams, Fds),
	'$check_select_time'(TimeOut, Sec, USec, socket_select(Socks, OutSocks, TimeOut, Streams, OutStreams)),
	'$socket_select'(Fds, Sec, USec, NFds),
	'$cp_socket_fds'(Socks, NFds, OutSocks, NFdsS),
	'$cp_stream_fds'(Streams, NFdsS, OutStreams).
	

/* check whether a list of options is valid */
'$check_list_for_sockets'(V,G) :- var(V), !,
	'$do_error'(instantiation_error,G).
'$check_list_for_sockets'([],_) :- !.
'$check_list_for_sockets'([_|T],G) :- !,
	'$check_list_for_sockets'(T,G).
'$check_list_for_sockets'(T,G) :-
	'$do_error'(type_error(list,T),G).

'$select_cp_fds'([], Fds, Fds).
'$select_cp_fds'([_-Fd|L], Fds0, [Fd|Fds]) :-
	'$select_cp_fds'(L, Fds0, Fds).

'$check_select_time'(V, _, _, Goal) :-
	var(V), !,
	'$do_error'(instantiation_error,Goal).
'$check_select_time'(off, -1, -1, _).
'$check_select_time'(Sec0:USec0, Sec, USec, _) :-
	Sec is Sec0,
	USec is USec0,
	Sec >= 0, USec >= 0.
	
'$cp_socket_fds'([], Fds, [], Fds).
'$cp_socket_fds'([_|Scks], [[]|Fds], Out, StrFds) :- !,
	'$cp_socket_fds'(Scks, Fds, Out, StrFds).
'$cp_socket_fds'([T-Socket|Scks], [Socket|Fds], [T-connection(Client,Stream)|Out], StrFds) :-
	socket_accept(Socket, Client, Stream),
	'$cp_socket_fds'(Scks, Fds, Out, StrFds).

'$cp_stream_fds'([], _, []).
'$cp_stream_fds'([_|Strs], [[]|Fds], Out) :- !,
	'$cp_stream_fds'(Strs, Fds, Out).
'$cp_stream_fds'([Stream|Strs], [Stream|Fds], [Stream|Out]) :-
	'$cp_stream_fds'(Strs, Fds, Out).

socket_buffering(Sock, Flag, InSize, OutSize) :-
	var(OutSize), OutSize \= InSize, !,
	'$do_error'(instantiation_error,socket_buffering(Sock, Flag, InSize, OutSize)).
socket_buffering(Sock, Flag, InSize, OutSize) :-
	'$convert_sock_buff'(OutSize, OutNumb),
	'$socket_buffering'(Sock, Flag, InNumb, OutNumb),
	'$convert_sock_buff'(InSize, InNumb).

'$convert_sock_buff'(V, V) :- var(V), !.
'$convert_sock_buff'(unbuf, 1) :- !.
'$convert_sock_buff'(fullbuf, _).



