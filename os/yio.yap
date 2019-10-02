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
* File:		yio.yap							 *
* Last rev:								 *
* mods:									 *
* comments:	Input output predicates			 		 *
*									 *
*************************************************************************/



:- system_module( '$_yio', [at_end_of_line/0,
        at_end_of_line/1,
        consult_depth/1,
        current_char_conversion/2,
        current_line_number/1,
        current_line_number/2,
        current_stream/3,
        display/1,
        display/2,
        exists/1,
        fileerrors/0,
        format/1,
        nofileerrors/0,
        open_pipe_streams/2,
        prolog_file_name/2,
        read/1,
        read/2,
        sformat/3,
        socket/2,
        socket/4,
        socket_connect/3,
        stream_position/2,
        stream_position/3,
        stream_position_data/3,
        ttyget/1,
        ttyget0/1,
        ttynl/0,
        ttyput/1,
        ttyskip/1,
	rename/2,
        write_depth/2], ['$default_expand'/1,
        '$extend_file_search_path'/1,
        '$set_default_expand'/1]).

:- use_system_module( '$_boot', ['$system_catch'/4]).

:- use_system_module( '$_errors', ['$do_error'/2]).

/** @defgroup InputOutput Input/Output Predicates
 ⁸ @ingroup builtins
 *
 * @{
 * 
 * Some of the Input/Output predicates described below will in certain conditions
 * provide error messages and abort only if the file_errors flag is set.
 * If this flag is cleared the same predicates will just fail. Details on
 * setting and clearing this flag are given under 7.7.
 * @}
 */

/* stream predicates							*/

/** @defgroup IOSockets YAP Old Style Socket and Pipe Interface
 * @ingroup InputOutput
 *      @{
 * 
 *     Autoload the socket/pipe library
 * 
 * */


/** @pred  socket(+ _DOMAIN_,- _SOCKET_)
 * 
 * Call socket/4 with  _TYPE_ bound to `SOCK_STREAM'` and
 *  _PROTOCOL_ bound to `0`.
 * 
 * 
*/

/** @pred  socket(+ _DOMAIN_,+ _TYPE_,+ _PROTOCOL_,- _SOCKET_)
 * 
 * Corresponds to the BSD system call `socket`. Create a socket for
 * domain  _DOMAIN_ of type  _TYPE_ and protocol
 *  _PROTOCOL_. Both  _DOMAIN_ and  _TYPE_ should be atoms,
 * whereas  _PROTOCOL_ must be an integer.
 * The new socket object is
 * accessible through a descriptor bound to the variable  _SOCKET_.
 * 
 * The current implementation of YAP  accepts socket
 * domains `AF_INET` and `AF_UNIX`.
 * Socket types depend on the
 * underlying operating system, but at least the following types are
 * supported: `SOCK_STREAM` and `SOCK_DGRAM` (untested in 6.3).
 * 
 * 
*/

/** @pred  socket_connect(+ _SOCKET_, + _PORT_, - _STREAM_)
 * 
 * Interface to system call `connect`, used for web clients: connect
 * socket  _SOCKET_ to  _PORT_. The connection results in the
 * read/write stream  _STREAM_.
 * 
 * Port information depends on the domain:
 * 
 * + 'AF_UNIX'(+ _FILENAME_)
 *    connect to socket at file  _FILENAME_.
 * 
 * + 'AF_INET'(+ _HOST_,+ _PORT_)
 *    Connect to socket at host  _HOST_ and port  _PORT_.
*/


/** @pred open_pipe_streams(Read, Write)
 * 
 *   Autoload old pipe access interface
 * 
*/

%! @}



%! @addtogroup Write
%%   @{

/* meaning of flags for '$write' is
	 1	quote illegal atoms
	 2	ignore operator declarations
	 4	output '$VAR'(N) terms as A, B, C, ...
	 8	use portray(_)

   flags are defined in yapio.h
*/

/** @pred  display(+ _T_)


Displays term  _T_ on the current output stream. All Prolog terms are
written in standard parenthesized prefix notation.


*/
display(T) :-
	   current_output(Out),
	   write_term(Out, T, [ignore_ops(true)]).

/** @pred  display(+ _S_, _T_)

Like display/1, but using stream  _S_ to display the term.


*/
display(Stream, T) :-
	   write_term(Stream, T, [ignore_ops(true)]).

/* interface to user portray	*/
'$portray'(T) :-
	\+ '$undefined'(portray(_),user),
	catch(user:portray(T),Error,'$Error'(Error)), !,
	set_value('$portray',true), fail.
'$portray'(_) :- set_value('$portray',false), fail.

%! @}

%! @addtogroup Format
%%   @{

/** @pred  format(+ _T_)

Print formatted output to the current output stream.


*/
format(T) :-
	format(T, []).

%! @}

/**
 * @addtogroup CharIO
 *
 *   @{
 *
 * @brief character I/O	
 */

/** @pred  ttyget(- _C_)


The same as `get(C)`, but from stream user_input.


*/
ttyget(N) :- get(user_input,N).

/** @pred  ttyget0(- _C_)


The same as `get0(C)`, but from stream user_input.


*/
ttyget0(N) :- get0(user_input,N).

/** @pred  ttyskip(- _C_)


Like skip/1, but always using stream user_input.
stream.


*/
ttyskip(N) :-  N1 is N, '$skip'(user_input,N1).

/** @pred  ttyput(+ _N_)


As `put(N)` but always to user_output.


*/
ttyput(N) :-  N1 is N, put(user_output,N1).

/** @pred  ttynl


Outputs a new line to stream user_output.

*/
ttynl :- nl(user_output).

%! @}

%! @addtogroup StreamM
%!   @{

/** @pred  current_line_number(- _LineNumber_)

Unify  _LineNumber_ with the line number for  the current output stream.

*/
current_line_number(N) :-
	current_input(Stream),
	line_count(Stream, N).

/** @pred  current_line_number(+ _Stream_,- _LineNumber_)

Unify  _LineNumber_ with the line number for  _Stream_.

*/
current_line_number(Stream,N) :-
	line_count(Stream, N).

/** @pred  stream_position(+ _Stream_,- _StreamPosition_)

Unify  _StreamPosition_ with the packaged information of position on
current stream  _Stream_. Use stream_position_data/3 to
retrieve information on character or line count.

*/
stream_position(Stream, Position) :-
	stream_property(Stream, position(Position)).

/** @pred  stream_position(+ _Stream_,- _StreamPosition_, +_NewPosition_)

Unify _StreamPosition_ with the packaged information of position on
current stream _Stream_ an then moves to position _NewPosition_.

*/
stream_position(Stream, Position, NewPosition) :-
	stream_property(Stream, position(Position)),
	set_stream_position(Stream, NewPosition).

/** @pred at_end_of_line

   Tests whether the next character in the current input stream is a line break character.
*/

at_end_of_line :-
	current_input(S),
	at_end_of_line(S).

/** @pred at_end_of_line( +Stream )

   Tests whether the next character in the stream is a line break character.
*/
at_end_of_line(S) :-
	stream_property(S, end_of_stream(past)), !.
at_end_of_line(S) :-
	peek_code(S,N), ( N = 10 -> true ; N = -1).

/** @pred  current_char_conversion(? _IN_,? _OUT_) is iso


If  _IN_ is unbound give all current character
translations. Otherwise, give the translation for  _IN_, if one
exists.


*/
current_char_conversion(X,Y) :-
	var(X), !,
	'$all_char_conversions'(List),
	'$fetch_char_conversion'(List,X,Y).
current_char_conversion(X,Y) :-
	'$current_char_conversion'(X,Y).


'$fetch_char_conversion'([X,Y|_],X,Y).
'$fetch_char_conversion'([_,_|List],X,Y) :-
	'$fetch_char_conversion'(List,X,Y).

split_path_file(File, Path, Name) :-
	file_directory_name(File, Path),
	file_base_name(File, Name).

%! @}

%! @addtogroup StreamM
%!   @{

/** @pred  current_stream( _F_, _M_, _S_)


Defines the relation: The stream  _S_ is opened on the file  _F_
in mode  _M_. It might be used to obtain all open streams (by
backtracking) or to access the stream for a file  _F_ in mode
 _M_, or to find properties for a stream  _S_. Notice that some
streams might not be associated to a file: in this case YAP tries to
return the file number. If that is not available, YAP unifies  _F_
with  _S_.
*/
current_stream(File, Mode, Stream) :-
    stream_property(Stream, mode(Mode)),
    '$stream_name'(Stream, File).

'$stream_name'(Stream, File) :-
    stream_property(Stream, file_name(File)), !.
'$stream_name'(Stream, file_no(File)) :-
    stream_property(Stream, file_no(File)), !.
'$stream_name'(Stream, Stream).

'$extend_file_search_path'(P) :-
	atom_codes(P,S),
	'$env_separator'(ES),
	'$split_for_path'(S,0'=,ES,Paths), %'
	'$add_file_search_paths'(Paths).

'$split_for_path'([], _, _, []).
'$split_for_path'(S, S1, S2, [A1=A2|R]) :-
	'$fetch_first_path'(S, S1, A1, SR1),
	'$fetch_second_path'(SR1, S2, A2, SR),
	'$split_for_path'(SR, S1, S2, R) .

'$fetch_first_path'([S1|SR],S1,[],SR) :- !.
'$fetch_first_path'([C|S],S1,[C|F],SR) :-
	'$fetch_first_path'(S,S1,F,SR).

'$fetch_second_path'([],_,[],[]).
'$fetch_second_path'([S1|SR],S1,[],SR) :- !.
'$fetch_second_path'([C|S],S1,[C|A2],SR) :-
	'$fetch_second_path'(S,S1,A2,SR).

'$add_file_search_paths'([]).
'$add_file_search_paths'([NS=DS|Paths]) :-
	atom_codes(N,NS),
	atom_codes(D,DS),
	assert(user:file_search_path(N,D)),
	'$add_file_search_paths'(Paths).


'$format@'(Goal,Out) :-
	with_output_to(codes(Out), Goal).

sformat(String, Form, Args) :-
	format(codes(String, []), Form, Args).


/** @pred  stream_position_data(+ _Field_,+ _StreamPosition_,- _Info_)
  Extract values from stream position objects.


  '$stream_position' is of the format '$stream_position'(Byte, Char,
  Line, LinePos).  Given the packaged stream position term
  _StreamPosition_, unify _Info_ with _Field_ `line_count`,
  `byte_count`, or `char_count`.

 */
stream_position_data(Prop, Term, Value) :-
        nonvar(Prop), !,
        (   '$stream_position_field'(Prop, Pos)
        ->  arg(Pos, Term, Value)
        ;   '$do_error'(domain_error(stream_position_data), Prop)
        ).
stream_position_data(Prop, Term, Value) :-
        '$stream_position_field'(Prop, Pos),
        arg(Pos, Term, Value).

'$stream_position_field'(char_count,    1).
'$stream_position_field'(line_count,    2).
'$stream_position_field'(line_position, 3).
'$stream_position_field'(byte_count,    4).

'$set_encoding'(Enc) :-
    set_stream(loop_stream, encoding(Enc)).

%! @}

/**
 * @defgroup FilesM File and Directory Operations
 * @ingroup InputOutput
 * @{
 *
 */
'$codes_to_chars'(String0, String, String0) :- String0 == String, !.
'$codes_to_chars'(String0, [Code|String], [Char|Chars]) :-
	atom_codes(Char, [Code]),
	'$codes_to_chars'(String0, String, Chars).



/** @pred  exists(+ _F_)

Checks if file  _F_ exists in the current directory.

*/
exists(F) :-
	absolute_file_name(F, _, [file_errors(fail),access(exist),expand(true)]).


/** @pred file_exists(+ _File__)

The atom  _File_ corresponds to an existing file or directory.


*/
file_exists(IFile) :-
	absolute_file_name(IFile, _File, [expand(true), solutions(first), access(exist)]).

/** @pred  rename(+F , +G)

      Renames the single file  _F_ to  _G_.
*/
rename(IFile, OFile) :-
	absolute_file_name(IFile, IF, [access(read),expand(true)]),
	absolute_file_name(OFile, OF, [expand(true)]),
	'$rename'(IF, OF).

/** @pred  access_file(+F , +G)

      Verify whether file F respects property _G_. The file is  processed
      with absolute_file_name.
*/
access_file(IFile, Access) :-
    absolute_file_name(IFile, _IF, [access(Access),expand(true)]).

/** @pred prolog_file_name( +File, -PrologFileaName)

Unify _PrologFileName_ with the Prolog file associated to _File_.

*/
prolog_file_name(File, PrologFileName) :-
        var(File), !,
        '$do_error'(instantiation_error, prolog_file_name(File, PrologFileName)).
prolog_file_name(user, Out) :- !, Out = user.
prolog_file_name(File, PrologFileName) :-
        atom(File), !,
        system:true_file_name(File, PrologFileName).
prolog_file_name(File, PrologFileName) :-
        '$do_error'(type_error(atom,File), prolog_file_name(File, PrologFileName)).


/**
@}
*/
