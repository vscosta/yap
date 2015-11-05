%
% Edinburgh IO.
				%

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

seeing(File) :- current_input(Stream),
	stream_property(Stream,file_name(NFile)),
	(
	 stream_property(user_input,file_name(NFile))
	->
	 File = user
	;
	 NFile = File
	).

seen :- current_input(Stream), close(Stream), set_input(user).

tell(user) :- !, set_output(user_output).
tell(F) :- var(F), !,
	'$do_error'(instantiation_error,tell(F)).
tell(F) :-
	current_output(Stream),
	stream_property(Stream,file_name(F)),
	!.
tell(F) :- current_stream(_,write,Stream), '$user_file_name'(Stream, F),  !,
	set_output(Stream).
tell(Stream) :-
	'$stream'(Stream),
	current_stream(_,write,Stream), !,
	set_output(Stream).
tell(F) :-
	open(F,write,Stream),
	set_output(Stream).
		
telling(File) :-
	current_output(Stream),
	stream_property(Stream,file_name(NFile)),
	( stream_property(user_output,file_name(NFile)) -> File = user ; File = NFile ).

told :- current_output(Stream),
	set_output(user),
	close(Stream).

