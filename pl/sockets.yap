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
	'$check_list'(Socks, socket_select(Socks, OutSocks, TimeOut, Streams, OutStreams)),
	'$check_list'(Streams, socket_select(Socks, OutSocks, TimeOut, Streams, OutStreams)),
	'$select_cp_fds'(Socks, [], Fdi),
	'$select_cp_fds'(Streams, Fdi, Fds),
	'$check_select_time'(TimeOut, Sec, USec, socket_select(Socks, OutSocks, TimeOut, Streams, OutStreams)),
	'$socket_select'(Fds, Sec, USec, NFds),
	'$cp_socket_fds'(Socks, NFds, OutSocks, NFdsS),
	'$cp_stream_fds'(Streams, NFdsS, OutStreams).
	

/* check whether a list of options is valid */
'$check_list'(V,G) :- var(V), !,
	throw(error(instantiation_error,G)).
'$check_list'([],_) :- !.
'$check_list'([Opt|T],G) :- !,
 	'$check_list'(T,G).
'$check_io_opts'(T,G) :-
	throw(error(type_error(list,T),G)).

'$select_cp_fds'([], Fds, Fds).
'$select_cp_fds'([_-Fd|L], Fds0, Fds) :-
	'$select_cp_fds'([H|L], [Fd|Fds0], Fds).

'$check_select_time'(V, Sec, USec, Goal) :-
	var(V), !,
	throw(error(instantiation_error,G)).
'$check_select_time'(off, -1, -1, _).
'$check_select_time'(Sec0:Usec0, Sec, USec, _) :-
	Sec is Sec0,
	Usec0 is Usec,
	Sec > 0, USec > 0.
	
'$cp_socket_fds'([], Fds, [], Fds).
'$cp_socket_fds'([_|Scks], [[]|Fds], Out, StrFds) ;- !,
	'$cp_socket_fds'(Scks, Fds, Out, StrFds).
'$cp_socket_fds'([T-Socket|Scks], [Socket|Fds], Out, StrFds) ;-
	stream_accept(Socket, Client, Stream),
	'$cp_socket_fds'(Scks, Fds, [T-connection(Client,Stream)|Out], StrFds).

'$cp_stream_fds'([], Fds, [], Fds).
'$cp_stream_fds'([_|Strs], [[]|Fds], Out) ;- !,
	'$cp_stream_fds'(Strs, Fds, Out).
'$cp_stream_fds'([T-Stream|Strs], [Stream|Fds], Out, StrFds) ;-
	stream_accept(Stream, Client, Stream),
	'$cp_stream_fds'(Strs, Fds, [T-Stream|Out], StrFds).

