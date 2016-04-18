%
% Edinburgh IO.
/**
 * @file   edio.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Wed Jan 20 01:07:02 2016
 *
 * @brief  Input/Output according to the DEC-10 Prolog. PLease consider using the ISO
 * standard predicates for new code.
 *
 *
*/


				%

/** @pred  see(+ _S_)


If  _S_ is a currently opened input stream then it is assumed to be
the current input stream. If  _S_ is an atom it is taken as a
filename. If there is no input stream currently associated with it, then
it is opened for input, and the new input stream thus created becomes
the current input stream. If it is not possible to open the file, an
error occurs.  If there is a single opened input stream currently
associated with the file, it becomes the current input stream; if there
are more than one in that condition, then one of them is chosen.

When  _S_ is a stream not currently opened for input, an error may be
reported, depending on the state of the `file_errors` flag. If
 _S_ is neither a stream nor an atom the predicates just fails.


*/
see(user) :- !, set_input(user_input).
see(F) :- var(F), !,
	'$do_error'(instantiation_error,see(F)).
see(F) :- current_input(Stream),
	'$user_file_name'(Stream,F).
see(F) :- current_stream(_,read,Stream), '$user_file_name'(Stream,F), !,
	set_input(Stream).
see(Stream) :- '$stream'(Stream), current_stream(_,read,Stream), !,
	set_input(Stream).
see(F) :- open(F,read,Stream), set_input(Stream).

/** @pred  seeing(- _S_)


The current input stream is unified with  _S_.


*/
seeing(File) :- current_input(Stream),
	stream_property(Stream,file_name(NFile)),
	(
	 stream_property(user_input,file_name(NFile))
	->
	 File = user
	;
	 NFile = File
	).

/** @pred  seen


Closes the current input stream, as opened by see/1. Standard input
stream goes to the original ùser_input`.

 */
seen :- current_input(Stream), close(Stream), set_input(user).

/** @pred  tell(+ _S_)


If  _S_ is a currently opened stream for output, it becomes the
current output stream. If  _S_ is an atom it is taken to be a
filename.  If there is no output stream currently associated with it,
then it is opened for output, and the new output stream created becomes
the current output stream. Existing files are clobbered, use append/1 to ext  end a file.
 If it is not possible to open the file, an
error occurs.  If there is a single opened output stream currently
associated with the file, then it becomes the current output stream; if
there are more than one in that condition, one of them is chosen.

Whenever  _S_ is a stream not currently opened for output, an error
may be reported, depending on the state of the file_errors flag. The
predicate just fails, if  _S_ is neither a stream nor an atom.


*/
tell(user) :- !, set_output(user_output).
tell(F) :- var(F), !,
	'$do_error'(instantiation_error,tell(F)).
tell(F) :-
	current_output(Stream),
	stream_property(Stream,file_name(F)),
	!.
tell(F) :-
	current_stream(_,write,Stream),
	'$user_file_name'(Stream, F),  !,
	set_output(Stream).
tell(Stream) :-
	'$stream'(Stream),
	current_stream(_,write,Stream), !,
	set_output(Stream).
tell(F) :-
	open(F,write,Stream),
	set_output(Stream).

  /** @pred  append(+ _S_)


  If  _S_ is a currently opened stream for output, it becomes the
  current output stream. If  _S_ is an atom it is taken to be a
  filename.  If there is no output stream currently associated with it,
  then it is opened for output in *append* mode, that is, by adding new data to the end of the file.
  The new output stream created becomes
  the current output stream. If it is not possible to open the file, an
  error occurs.  If there is a single opened output stream currently
  associated with the file, then it becomes the current output stream; if
  there are more than one in that condition, one of them is chosen.

  Whenever  _S_ is a stream not currently opened for output, an error
  may be reported, depending on the state of the file_errors flag. The
  predicate just fails, if  _S_ is neither a stream nor an atom.


  */
  tell(user) :- !, set_output(user_output).
  tell(F) :- var(F), !,
  	'$do_error'(instantiation_error,tell(F)).
  tell(F) :-
  	current_output(Stream),
  	stream_property(Stream,file_name(F)),
  	!.
  tell(F) :-
  	current_stream(_,write,Stream),
  	'$user_file_name'(Stream, F),  !,
  	set_output(Stream).
  tell(Stream) :-
  	'$stream'(Stream),
  	current_stream(_,write,Stream), !,
  	set_output(Stream).
  tell(F) :-
  	open(F,write,Stream),
  	set_output(Stream).

/** @pred  telling(- _S_)


The current output stream is unified with  _S_.


*/
telling(File) :-
	current_output(Stream),
	stream_property(Stream,file_name(NFile)),
	( stream_property(user_output,file_name(NFile)) -> File = user ; File = NFile ).

/** @pred  told


Closes the current output stream, and the user's terminal becomes again
the current output stream. It is important to remember to close streams
after having finished using them, as the maximum number of
simultaneously opened streams is 17.


*/
told :- current_output(Stream),
        !,
	set_output(user_output),
	close(Stream).
