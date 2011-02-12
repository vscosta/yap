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

/* stream predicates							*/

close(V) :- var(V), !,
	'$do_error'(instantiation_error,close(V)).
close(File) :-
	atom(File), !,
	(
	    '$access_yap_flags'(8, 0),
	    current_stream(_,_,Stream),
	    '$user_file_name'(Stream,File)
        ->
	    '$close'(Stream)
	;
	    '$close'(File)
	).
close(Stream) :-
	'$close'(Stream).

close(V,Opts) :- var(V), !,
	'$do_error'(instantiation_error,close(V,Opts)).
close(S,Opts) :-
	'$check_io_opts'(Opts,close(S,Opts)),
	/* YAP ignores the force/1 flag */ 
	close(S).
	
/* check whether a list of options is valid */
'$check_io_opts'(V,G) :- var(V), !,
	'$do_error'(instantiation_error,G).
'$check_io_opts'([],_) :- !.
'$check_io_opts'([H|_],G) :- var(H), !,
	'$do_error'(instantiation_error,G).
'$check_io_opts'([Opt|T],G) :- !,
	'$check_opt'(G,Opt,G),
	'$check_io_opts'(T,G).
'$check_io_opts'(T,G) :-
	'$do_error'(type_error(list,T),G).

'$check_opt'(close(_,_),Opt,G) :- !,
	(Opt = force(X) ->
	    '$check_force_opt_arg'(X,G) ;
	    '$do_error'(domain_error(close_option,Opt),G)
	).
'$check_opt'(read_term(_,_),Opt,G) :-
	'$check_opt_read'(Opt, G).
'$check_opt'(stream_property(_,_),Opt,G) :-
	'$check_opt_sp'(Opt, G).
'$check_opt'(write_term(_,_),Opt,G) :-
	'$check_opt_write'(Opt, G).
'$check_opt'(yap_flag(_,_),Opt,G) :-
	'$check_opt_write'(Opt, G).

'$check_opt_read'(variables(_), _) :- !.
'$check_opt_read'(variable_names(_), _) :- !.
'$check_opt_read'(singletons(_), _) :- !.
'$check_opt_read'(syntax_errors(T), G) :- !,
	'$check_read_syntax_errors_arg'(T, G).
'$check_opt_read'(term_position(_), _) :- !.
'$check_opt_read'(module(_), _) :- !.
'$check_opt_read'(A, G) :-
	'$do_error'(domain_error(read_option,A),G).

'$check_opt_sp'(file_name(_), _) :- !.
'$check_opt_sp'(mode(_), _) :- !.
'$check_opt_sp'(input, _) :- !.
'$check_opt_sp'(output, _) :- !.
'$check_opt_sp'(alias(_), _) :- !.
'$check_opt_sp'(position(_), _) :- !.
'$check_opt_sp'(end_of_stream(_), _) :- !.
'$check_opt_sp'(eof_action(_), _) :- !.
'$check_opt_sp'(reposition(_), _) :- !.
'$check_opt_sp'(type(_), _) :- !.
'$check_opt_sp'(bom(_), _) :- !.
'$check_opt_sp'(encoding(_), _) :- !.
'$check_opt_sp'(representation_errors(_), _) :- !.
'$check_opt_sp'(A, G) :-
	'$do_error'(domain_error(stream_property,A),G).

'$check_opt_write'(attributes(T), G) :- !,
	'$check_write_attributes'(T, G).
'$check_opt_write'(cycles(T), G) :- !,
	'$check_boolean'(T, write_option, cycles(T), G).
'$check_opt_write'(quoted(T), G) :- !,
	'$check_boolean'(T, write_option, quoted(T), G).
'$check_opt_write'(ignore_ops(T), G) :- !,
	'$check_boolean'(T, write_option, ignore_ops(T), G).
'$check_opt_write'(max_depth(T), G) :- !,
	'$check_write_max_depth'(T, G).
'$check_opt_write'(numbervars(T), G) :- !,
	'$check_boolean'(T, write_option, ignore_ops(T), G).
'$check_opt_write'(portrayed(T), G) :- !,
	'$check_boolean'(T, write_option, portrayed(T), G).
'$check_opt_write'(portray(T), G) :- !,
	'$check_boolean'(T, write_option, portray(T), G).
'$check_opt_write'(priority(T), G) :- !,
	'$check_priority_arg'(T, G).
'$check_opt_write'(swi(T), G) :- !,
	'$check_boolean'(T, write_option, swi(T), G).
'$check_opt_write'(A, G) :-
	'$do_error'(domain_error(write_option,A),G).

%
% check force arg
%
'$check_force_opt_arg'(X,G) :- var(X), !,
	'$do_error'(instantiation_error,G).
'$check_force_opt_arg'(true,_) :- !.
'$check_force_opt_arg'(false,_) :- !.
'$check_force_opt_arg'(X,G) :-
	'$do_error'(domain_error(close_option,force(X)),G).

'$check_read_syntax_errors_arg'(X, G) :- var(X), !,
	'$do_error'(instantiation_error,G).
'$check_read_syntax_errors_arg'(dec10,_) :- !.
'$check_read_syntax_errors_arg'(fail,_) :- !.
'$check_read_syntax_errors_arg'(error,_) :- !.
'$check_read_syntax_errors_arg'(quiet,_) :- !.
'$check_read_syntax_errors_arg'(X,G) :-
	'$do_error'(domain_error(read_option,syntax_errors(X)),G).

'$check_write_attributes'(X, G) :- var(X), !,
	'$do_error'(instantiation_error,G).
'$check_write_attributes'(ignore,_) :- !.
'$check_write_attributes'(dots,_) :- !.
'$check_write_attributes'(write,_) :- !.
'$check_write_attributes'(portray,_) :- !.
'$check_write_attributes'(X,G) :-
	'$do_error'(domain_error(write_option,attributes(X)),G).

'$check_boolean'(X, _, _, G) :- var(X), !,
	'$do_error'(instantiation_error,G).
'$check_boolean'(true,_,_,_) :- !.
'$check_boolean'(false,_,_,_) :- !.
'$check_boolean'(X,B,T,G) :-
	'$do_error'(domain_error(B,T),G).

'$check_write_max_depth'(X, G) :- var(X), !,
	'$do_error'(instantiation_error,G).
'$check_write_max_depth'(I,_) :- integer(I), I >= 0, !.
'$check_write_max_depth'(X,G) :-
	'$do_error'(domain_error(write_option,max_depth(X)),G).

'$check_priority_arg'(X, G) :- var(X), !,
	'$do_error'(instantiation_error,G).
'$check_priority_arg'(I,_) :- integer(I), I >= 0, I =< 1200, !.
'$check_priority_arg'(X,G) :-
	'$do_error'(domain_error(write_option,priority(X)),G).

set_input(Stream) :-
	'$set_input'(Stream).
	
set_output(Stream) :-
	'$set_output'(Stream).

open_null_stream(S) :- '$open_null_stream'(S).

open_pipe_streams(P1,P2) :- '$open_pipe_stream'(P1, P2).

fileerrors :- set_value(fileerrors,1).
nofileerrors :- set_value(fileerrors,0).

exists(F) :- access_file(F,exist).

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
	'$user_file_name'(Stream,NFile),
	( '$user_file_name'(user_input,NFile) -> File = user ; NFile = File).

seen :- current_input(Stream), '$close'(Stream), set_input(user).

tell(user) :- !, set_output(user_output).
tell(F) :- var(F), !,
	'$do_error'(instantiation_error,tell(F)).
tell(F) :- current_output(Stream),
	'$user_file_name'(Stream,F), !.
tell(F) :- current_stream(_,write,Stream), '$user_file_name'(Stream, F),  !,
	set_output(Stream).
tell(Stream) :- '$stream'(Stream), current_stream(_,write,Stream), !,
	set_output(Stream).
tell(F) :- open(F,write,Stream), set_output(Stream).
		
telling(File) :- current_output(Stream),
	'$user_file_name'(Stream,NFile),
	( '$user_file_name'(user_output,NFile) -> File = user ; File = NFile ).

told :- current_output(Stream), '$close'(Stream), set_output(user).


/* Term IO	*/

read(T) :-
	'$read'(false,T,_,_,_,Err),
	(nonvar(Err) ->
	    print_message(error,Err), fail
	    ;
	    true
	).

read(Stream,T) :-
	'$read'(false,T,_,_,_,Err,Stream),
	(nonvar(Err) ->
	    print_message(error,Err), fail
	    ;
	    true
	).

read_term(T, Options) :-
	'$check_io_opts'(Options,read_term(T, Options)),
	current_input(S),
	'$preprocess_read_terms_options'(Options,Module),
	'$read_vars'(S,T,Module,Pos,VL),
	'$postprocess_read_terms_options'(Options, T, VL, Pos).

read_term(Stream, T, Options) :-
	'$check_io_opts'(Options,read_term(T, Options)),
	'$preprocess_read_terms_options'(Options,Module),
	'$read_vars'(Stream,T,Module,Pos,VL),
	'$postprocess_read_terms_options'(Options, T, VL, Pos).

%
% support flags to read
%
'$preprocess_read_terms_options'([],_).
'$preprocess_read_terms_options'([syntax_errors(NewVal)|L],Mod) :- !,
	'$get_read_error_handler'(OldVal),
	set_value('$read_term_error_handler', OldVal),
	'$set_read_error_handler'(NewVal),
	'$preprocess_read_terms_options'(L,Mod).
'$preprocess_read_terms_options'([module(Mod)|L],Mod) :- !,
	'$preprocess_read_terms_options'(L,Mod).
'$preprocess_read_terms_options'([_|L],Mod) :-
	'$preprocess_read_terms_options'(L,Mod).

'$postprocess_read_terms_options'([], _, _, _).
'$postprocess_read_terms_options'([H|Tail], T, VL, Pos) :- !,
	'$postprocess_read_terms_option'(H, T, VL, Pos),
	'$postprocess_read_terms_options_list'(Tail, T, VL, Pos).
	
'$postprocess_read_terms_options_list'([], _, _, _).
'$postprocess_read_terms_options_list'([H|Tail], T, VL, Pos) :-
	'$postprocess_read_terms_option'(H, T, VL, Pos),
	'$postprocess_read_terms_options_list'(Tail, T, VL, Pos).

'$postprocess_read_terms_option'(syntax_errors(_), _, _, _) :-
	get_value('$read_term_error_handler', OldVal),
	'$set_read_error_handler'(OldVal).
'$postprocess_read_terms_option'(variable_names(Vars), _, VL, _) :-
	'$read_term_non_anonymous'(VL, Vars).
'$postprocess_read_terms_option'(singletons(Val), T, VL, _) :-
	'$singletons_in_term'(T, Val1),
	'$fetch_singleton_names'(Val1,VL,Val).
'$postprocess_read_terms_option'(variables(Val), T, _, _) :-
	'$variables_in_term'(T, [], Val).
'$postprocess_read_terms_option'(term_position(Pos), _, _, Pos).
'$postprocess_read_terms_option'(module(_), _, _, _).
%'$postprocess_read_terms_option'(cycles(Val), _, _).

'$read_term_non_anonymous'([], []).
'$read_term_non_anonymous'([[S|V]|VL], [Name=V|Vars]) :-
	atom_codes(Name,S),
	'$read_term_non_anonymous'(VL, Vars).


% problem is what to do about _ singletons.
% no need to do ordering, the two lists already come ordered.
'$fetch_singleton_names'([], _, []).
'$fetch_singleton_names'([_|_], [], []) :- !.
'$fetch_singleton_names'([V1|Ss], [[Na|V2]|Ns], ONs) :-
	V1 == V2, !,
	'$add_singleton_if_no_underscore'(Na,V2,NSs,ONs),
	'$fetch_singleton_names'(Ss, Ns, NSs).
'$fetch_singleton_names'([V1|Ss], [[_|V2]|Ns], NSs) :-
	V1 @> V2, !,
	'$fetch_singleton_names'([V1|Ss], Ns, NSs).
'$fetch_singleton_names'([_V1|Ss], Ns, NSs) :-
%	V1 @> V2,
	'$fetch_singleton_names'(Ss, Ns, NSs).

'$add_singleton_if_no_underscore'([95|_],_,NSs,NSs) :- !.
'$add_singleton_if_no_underscore'(Na,V2,NSs,[(Name=V2)|NSs]) :-
	atom_codes(Name, Na).

nl(Stream) :- '$put'(Stream,10).

nl :- current_output(Stream), '$put'(Stream,10), fail.
nl.

/* meaning of flags for '$write' is
	 1	quote illegal atoms
	 2	ignore operator declarations
	 4	output '$VAR'(N) terms as A, B, C, ...
	 8	use portray(_)

   flags are defined in yapio.h
*/

write(T) :- '$write'(4, T).

writeln(T) :-
	'$write'(4, T),
	nl.	

write(Stream,T) :- 
	'$write'(Stream,4,T).

writeq(T) :- '$write'(5,T).

writeq(Stream,T) :-
	'$write'(Stream,5,T),
	fail.
writeq(_,_).

display(T) :- '$write'(2,T).

display(Stream,T) :-
	'$write'(Stream,2,T),
	fail.
display(_,_).

write_canonical(T) :- '$write'(3,T).

write_canonical(Stream,T) :-
	'$write'(Stream,3,T),
	fail.
write_canonical(_,_).

print(T) :- '$write'(12,T), fail.
print(_).

print(Stream,T) :-
	'$write'(Stream,12,T),
	fail.
print(_,_).


write_term(T,Opts) :-
	'$check_io_opts'(Opts, write_term(T,Opts)),
	'$process_wt_opts'(Opts, 0, Flag, Priority, Callbacks),
	'$write_with_prio'(Flag, Priority, T),
	'$process_wt_callbacks'(Callbacks),
	fail.
write_term(_,_).

write_term(S, T, Opts) :-
	'$check_io_opts'(Opts, write_term(T,Opts)),
	'$process_wt_opts'(Opts, 0, Flag, Priority, Callbacks),
	'$write_with_prio'(S, Flag, Priority, T),
	'$process_wt_callbacks'(Callbacks),
	fail.
write_term(_,_,_).


'$process_wt_opts'([], Flag, Flag, 1200, []).
'$process_wt_opts'([quoted(true)|Opts], Flag0, Flag, Priority, CallBacks) :-
	FlagI is Flag0 \/ 0x01,
	'$process_wt_opts'(Opts, FlagI, Flag, Priority, CallBacks).
'$process_wt_opts'([quoted(false)|Opts], Flag0, Flag, Priority, CallBacks) :-
	FlagI is Flag0 /\ \0x01,
	'$process_wt_opts'(Opts, FlagI, Flag, Priority, CallBacks).
'$process_wt_opts'([ignore_ops(true)|Opts], Flag0, Flag, Priority, CallBacks) :-
	FlagI is Flag0 \/ 0x02,
	'$process_wt_opts'(Opts, FlagI, Flag, Priority, CallBacks).
'$process_wt_opts'([ignore_ops(false)|Opts], Flag0, Flag, Priority, CallBacks) :-
	FlagI is Flag0 /\ \0x02,
	'$process_wt_opts'(Opts, FlagI, Flag, Priority, CallBacks).
'$process_wt_opts'([numbervars(true)|Opts], Flag0, Flag, Priority, CallBacks) :-
	FlagI is Flag0 \/ 0x04,
	'$process_wt_opts'(Opts, FlagI, Flag, Priority, CallBacks).
'$process_wt_opts'([numbervars(false)|Opts], Flag0, Flag, Priority, CallBacks) :-
	FlagI is Flag0 /\ \0x04,
	'$process_wt_opts'(Opts, FlagI, Flag, Priority, CallBacks).
'$process_wt_opts'([portrayed(true)|Opts], Flag0, Flag, Priority, CallBacks) :-
	FlagI is Flag0 \/ 0x08,
	'$process_wt_opts'(Opts, FlagI, Flag, Priority, CallBacks).
'$process_wt_opts'([portrayed(false)|Opts], Flag0, Flag, Priority, CallBacks) :-
	FlagI is Flag0 /\ \0x08,
	'$process_wt_opts'(Opts, FlagI, Flag, Priority, CallBacks).
'$process_wt_opts'([portray(true)|Opts], Flag0, Flag, Priority, CallBacks) :-
	FlagI is Flag0 \/ 0x08,
	'$process_wt_opts'(Opts, FlagI, Flag, Priority, CallBacks).
'$process_wt_opts'([portray(false)|Opts], Flag0, Flag, Priority, CallBacks) :-
	FlagI is Flag0 /\ \0x08,
	'$process_wt_opts'(Opts, FlagI, Flag, Priority, CallBacks).
'$process_wt_opts'([cycles(false)|Opts], Flag0, Flag, Priority, CallBacks) :-
	FlagI is Flag0 \/ 0x20,
	'$process_wt_opts'(Opts, FlagI, Flag, Priority, CallBacks).
'$process_wt_opts'([cycles(false)|Opts], Flag0, Flag, Priority, CallBacks) :-
	FlagI is Flag0 /\ \0x20,
	'$process_wt_opts'(Opts, FlagI, Flag, Priority, CallBacks).
'$process_wt_opts'([swi(true)|Opts], Flag0, Flag, Priority, CallBacks) :-
	FlagI is Flag0 \/ 0x40,
	'$process_wt_opts'(Opts, FlagI, Flag, Priority, CallBacks).
'$process_wt_opts'([swi(false)|Opts], Flag0, Flag, Priority, CallBacks) :-
	FlagI is Flag0 /\ \0x40,
	'$process_wt_opts'(Opts, FlagI, Flag, Priority, CallBacks).
'$process_wt_opts'([attributes(_)|Opts], Flag0, Flag, Priority, CallBacks) :-
	'$process_wt_opts'(Opts, FlagI, Flag, Priority, CallBacks).
'$process_wt_opts'([priority(Priority)|Opts], Flag0, Flag, Priority, CallBacks) :-
	'$process_wt_opts'(Opts, Flag0, Flag, _, CallBacks).
'$process_wt_opts'([max_depth(D)|Opts], Flag0, Flag, Priority, [max_depth(D1,D0,D2)|CallBacks]) :-
	write_depth(D1,D0,D2),
	D10 is D*10,
	write_depth(D,D,D10),
	'$process_wt_opts'(Opts, Flag0, Flag, Priority, CallBacks).

'$process_wt_callbacks'([]).
'$process_wt_callbacks'([max_depth(D1,D0,D2)|Cs]) :-
	write_depth(D1,D0,D2),
	'$process_wt_callbacks'(Cs).


format(T) :-
	format(T, []).


/* interface to user portray	*/
'$portray'(T) :-
	\+ '$undefined'(portray(_),user),
	'$system_catch'(call(portray(T)),user,Error,user:'$Error'(Error)), !,
	set_value('$portray',true), fail.
'$portray'(_) :- set_value('$portray',false), fail.

/* character I/O	*/

get(N) :- current_input(S), get(S,N).

get_byte(V) :-
	\+ var(V), (\+ integer(V) ; V < -1 ; V > 256), !,
	'$do_error'(type_error(in_byte,V),get_byte(V)).
get_byte(V) :-
	current_input(S), 
	'$get_byte'(S,V).

get_byte(S,V) :-
	\+ var(V), (\+ integer(V) ; V < -1 ; V > 256), !,
	'$do_error'(type_error(in_byte,V),get_byte(S,V)).
get_byte(S,V) :-
	'$get_byte'(S,V).

peek_byte(V) :-
	\+ var(V), (\+ integer(V) ; V < -1 ; V > 256), !,
	'$do_error'(type_error(in_byte,V),get_byte(V)).
peek_byte(V) :-
	current_input(S), 
	'$peek_byte'(S,V).

peek_byte(S,V) :-
	\+ var(V), (\+ integer(V) ; V < -1 ; V > 256), !,
	'$do_error'(type_error(in_byte,V),get_byte(S,V)).
peek_byte(S,V) :-
	'$peek_byte'(S,V).

get_char(V) :-
	\+ var(V),
	( atom(V)  -> atom_codes(V,[_,_|_]), V \= end_of_file ; true ), !,
	'$do_error'(type_error(in_character,V),get_char(V)).
get_char(V) :-
	current_input(S),
	get0(S,I),
	( I = -1 -> V = end_of_file ; atom_codes(V,[I])).

get_char(S,V) :-
	\+ var(V),
	( atom(V)  -> atom_codes(V,[_,_|_]), V \= end_of_file ; true ), !,
	'$do_error'(type_error(in_character,V),get_char(S,V)).
get_char(S,V) :-
	get0(S,I),
	( I = -1 -> V = end_of_file ; atom_codes(V,[I])).

peek_char(V) :-
	\+ var(V),
	( atom(V)  -> atom_codes(V,[_,_|_]), V \= end_of_file ; true ), !,
	'$do_error'(type_error(in_character,V),get_char(V)).
peek_char(V) :-
	current_input(S),
	'$peek'(S,I),
	( I = -1 -> V = end_of_file ; atom_codes(V,[I])).

peek_char(S,V) :-
	\+ var(V),
	( atom(V)  -> atom_codes(V,[_,_|_]), V \= end_of_file ; true ), !,
	'$do_error'(type_error(in_character,V),get_char(S,V)).
peek_char(S,V) :-
	'$peek'(S,I),
	( I = -1 -> V = end_of_file ; atom_codes(V,[I])).

get_code(S,V) :-
	\+ var(V), (\+ integer(V)), !,
	'$do_error'(type_error(in_character_code,V),get_code(S,V)).
get_code(S,V) :-
	get0(S,V).

get_code(V) :-
	\+ var(V), (\+ integer(V)), !,
	'$do_error'(type_error(in_character_code,V),get_code(V)).
get_code(V) :-
	current_input(S),
	get0(S,V).

peek_code(S,V) :-
	\+ var(V), (\+ integer(V)), !,
	'$do_error'(type_error(in_character_code,V),get_code(S,V)).
peek_code(S,V) :-
	'$peek'(S,V).

peek_code(V) :-
	\+ var(V), (\+ integer(V)), !,
	'$do_error'(type_error(in_character_code,V),get_code(V)).
peek_code(V) :-
	current_input(S),
	'$peek'(S,V).

put_byte(V) :- var(V), !,
	'$do_error'(instantiation_error,put_byte(V)).
put_byte(V) :-
	(\+ integer(V) ; V < 0 ; V > 256), !,
	'$do_error'(type_error(byte,V),put_byte(V)).
put_byte(V) :-
	current_output(S), 
	'$put_byte'(S,V).


put_byte(S,V) :- var(V), !,
	'$do_error'(instantiation_error,put_byte(S,V)).
put_byte(S,V) :-
	(\+ integer(V) ; V < 0 ; V > 256), !,
	'$do_error'(type_error(byte,V),put_byte(S,V)).
put_byte(S,V) :-
	'$put_byte'(S,V).

put_char(V) :- var(V), !,
	'$do_error'(instantiation_error,put_char(V)).
put_char(V) :-
	( atom(V)  -> atom_codes(V,[_,_|_]) ; true ), !,
	'$do_error'(type_error(character,V),put_char(V)).
put_char(V) :-
	current_output(S),
	atom_codes(V,[I]),
	'$put'(S,I).

put_char(S,V) :- var(V), !,
	'$do_error'(instantiation_error,put_char(S,V)).
put_char(S,V) :-
	( atom(V)  -> atom_codes(V,[_,_|_]) ; true ), !,
	'$do_error'(type_error(character,V),put_char(S,V)).
put_char(S,V) :-
	atom_codes(V,[I]),
	'$put'(S,I).

put_code(V) :- var(V), !,
	'$do_error'(instantiation_error,put_code(V)).
put_code(V) :-
	(\+ integer(V)), !,
	'$do_error'(type_error(character_code,V),put_code(V)).
put_code(V) :-
	current_output(S), 
	'$put'(S,V).


put_code(S,V) :- var(V), !,
	'$do_error'(instantiation_error,put_code(S,V)).
put_code(S,V) :-
	(\+ integer(V)), !,
	'$do_error'(type_error(character_code,V),put_code(S,V)).
put_code(S,V) :-
	'$put'(S,V).



get0(N) :- current_input(S), get0(S,N).

put(N) :- current_output(S),  N1 is N, '$put'(S,N1).

put(Stream,N) :-  N1 is N, '$put'(Stream,N1).

skip(N) :- current_input(S),  N1 is N, '$skip'(S,N1).

skip(Stream,N) :- N1 is N, '$skip'(Stream,N1).

'$tab'(N) :- N<1, !.

'$tab'(N) :- put(32), N1 is N-1, '$tab'(N1).

tab(N) :- '$tab'(N), fail.
tab(_).

'$tab'(_,N) :- N<1, !.
'$tab'(Stream,N) :- put(Stream,32), N1 is N-1, '$tab'(Stream,N1).

tab(Stream,N) :- '$tab'(Stream,N), fail.
tab(_,_).

ttyget(N) :- get(user_input,N).

ttyget0(N) :- get0(user_input,N).

ttyskip(N) :-  N1 is N, '$skip'(user_input,N1).

ttyput(N) :-  N1 is N, '$put'(user_output,N1).

ttynl :- nl(user_output).

ttyflush :- flush_output(user_output).

flush_output :-
	current_output(Stream),
	flush_output(Stream).

current_line_number(N) :-
	current_input(Stream), '$current_line_number'(Stream,N).

current_line_number(user,N) :- !,
	'$current_line_number'(user_input,N).
current_line_number(A,N) :- 
	atom(A),
	current_stream(_,_,S), '$user_file_name'(S,A), !,
	'$current_line_number'(S,N).
current_line_number(S,N) :-
	'$current_line_number'(S,N).

line_count(Stream,N) :- current_line_number(Stream,N).

character_count(user,N) :- !,
	'$character_count'(user_input,N).
character_count(A,N) :- 
	atom(A),
	current_stream(_,_,S), '$user_file_name'(S,A), !,
	'$character_count'(S,N).
character_count(S,N) :-
	'$character_count'(S,N).

line_position(user,N) :- !,
	'$line_position'(user_input,N).
line_position(A,N) :- 
	atom(A),
	current_stream(_,_,S), '$user_file_name'(S,A), !,
	'$line_position'(S,N).
line_position(S,N) :-
	'$line_position'(S,N).

stream_position(user,N) :- !,
	'$show_stream_position'(user_input,N).
stream_position(A,N) :- 
	atom(A),
	'$current_stream'(_,_,S), '$user_file_name'(S,A), !,
	'$show_stream_position'(S,N).
stream_position(S,N) :-
	'$show_stream_position'(S,N).

stream_position(user,N,M) :- !,
	'$stream_position'(user_input,N,M).
stream_position(A,N,M) :- 
	atom(A),
	'$current_stream'(_,_,S), '$user_file_name'(S,A), !,
	'$stream_position'(S,N,M).
stream_position(S,N,M) :-
	'$stream_position'(S,N,M).

'$stream_position'(S,N,M) :-
	var(M), !,
	'$show_stream_position'(S,N),
	M = N.
'$stream_position'(S,N,M) :-
	'$show_stream_position'(S,N),
	'$set_stream_position'(S,M).


set_stream_position(S,N) :- var(S), !,
	'$do_error'(instantiation_error, set_stream_position(S, N)).
set_stream_position(user,N) :- !,
	'$set_stream_position'(user_input,N).
set_stream_position(A,N) :- 
	atom(A),
	'$current_stream'(_,_,S), '$user_file_name'(S,A), !,
	'$set_stream_position'(S,N).
set_stream_position(S,N) :-
	'$set_stream_position'(S,N).

'$show_stream_eof'(Stream, past) :-
	'$past_eof'(Stream), !.
'$show_stream_eof'(Stream, at) :-
	'$peek'(Stream,N), N = -1, !.
'$show_stream_eof'(_, not).
	
'$show_stream_eof_action'(Fl, error) :-
	Fl /\ 0x0200 =:= 0x0200, !.
'$show_stream_eof_action'(Fl, reset) :-
	Fl /\ 0x0400 =:= 0x0400, !.
'$show_stream_eof_action'(_, eof_code).

'$show_stream_reposition'(Fl, true) :-
	Fl /\ 0x2000 =:= 0x2000, !.
'$show_stream_reposition'(_, false).

'$show_stream_bom'(Fl, true) :-
	'$has_bom'(Fl), !.
'$show_stream_bom'(_, false).

'$show_stream_type'(Fl, binary) :-
	Fl /\ 0x0100 =:= 0x0100, !.
'$show_stream_type'(_, text).

at_end_of_stream :-
	current_input(S),
	at_end_of_stream(S).

at_end_of_stream(S) :-
	'$past_eof'(S), !.
at_end_of_stream(S) :-
	'$peek'(S,N), N = -1.


at_end_of_line :-
	current_input(S),
	at_end_of_line(S).

at_end_of_line(S) :-
	'$past_eof'(S), !.
at_end_of_line(S) :-
	'$peek'(S,N), ( N = 10 -> true ; N = -1).


consult_depth(LV) :- '$show_consult_level'(LV).

current_char_conversion(X,Y) :-
	var(X), !,
	'$all_char_conversions'(List),
	'$fetch_char_conversion'(List,X,Y).
current_char_conversion(X,Y) :-
	'$current_char_conversion'(X,Y).


'$fetch_char_conversion'([X,Y|_],X,Y).
'$fetch_char_conversion'([_,_|List],X,Y) :-
	'$fetch_char_conversion'(List,X,Y).


current_stream(File, Opts, Stream) :-
	'$current_stream'(File, Opts, Stream).

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
	'$with_output_to_chars'(Goal, _, [], Out).

'$with_output_to_chars'(Goal, Stream, L0, Chars) :-
	charsio:open_mem_write_stream(Stream),
	current_output(SO),
	set_output(Stream),
	'$do_output_to_chars'(Goal, Stream, L0, Chars, SO).

'$do_output_to_chars'(Goal, Stream, L0, Chars, SO) :-
	catch(Goal, Exception, '$handle_exception'(Exception,Stream,SO)),
	!,
	set_output(SO),
	charsio:peek_mem_write_stream(Stream, L0, Chars),
	close(Stream).
'$do_output_to_chars'(_Goal, Stream, _L0, _Chars, SO) :-
	set_output(SO),
	close(Stream),
	fail.

sformat(String, Form, Args) :-
	charsio:open_mem_write_stream(Stream),
	format(Stream, Form, Args),
	charsio:peek_mem_write_stream(Stream, [], String),
	close(Stream).


'$handle_exception'(Exception, Stream, SO) :-
	set_output(SO),
	close(Stream),
	throw(Exception).

write_depth(T,L) :- write_depth(T,L,_).

is_stream(S) :-
	catch('$check_stream'(S), _, fail), !.

stream_position_data(line_count, '$stream_position'(_,Data,_,_,_), Data).
stream_position_data(line_position, '$stream_position'(_,_,Data,_,_), Data).
%stream_position_data(char_count, '$stream_position'(Data,_,_,_,_), Data).
stream_position_data(byte_count, '$stream_position'(Data,_,_,_,_), Data).

'$default_expand'(Expand) :-
	nb_getval('$open_expands_filename',Expand).

'$set_default_expand'(true) :- !,
	nb_setval('$open_expands_filename',true).
'$set_default_expand'(false) :- !,
	nb_setval('$open_expands_filename',false).
'$set_default_expand'(V) :- !,
	'$do_error'(domain_error(flag_value,V),yap_flag(open_expands_file_name,X)).

prolog_file_name(File, PrologFileName) :-
	var(File), !,
	'$do_error'(instantiation_error, prolog_file_name(File, PrologFileName)).
prolog_file_name(user, Out) :- !, Out = user.
prolog_file_name(File, PrologFileName) :-
	atom(File), !,
	operating_system_support:true_file_name(File, PrologFileName).
prolog_file_name(File, PrologFileName) :-
	'$do_error'(type_error(atom,T), prolog_file_name(File, PrologFileName)).

with_output_to(Output, Command) :-
	setup_call_cleanup( '$setup_wot'(Output, Stream, OldStream, with_output_to(Output, Command)),
			    once(Command),
			    '$cleanup_wot'(Output, Stream, OldStream) ).

'$setup_wot'(Output, Stream, OldStream, Goal) :-
	'$setup_wot'(Output, Stream, Goal),
	current_output(OldStream),
	set_output(Stream).

'$setup_wot'(Output, Stream, Goal) :-
	var(Output), !,
	'$do_error'(instantiation_error,Goal).
'$setup_wot'(atom(_Atom), Stream, _) :- !,
	charsio:open_mem_write_stream(Stream).
'$setup_wot'(codes(_Codes), Stream, _) :- !,
	charsio:open_mem_write_stream(Stream).
'$setup_wot'(codes(_Codes, _Tail), Stream, _) :- !,
	charsio:open_mem_write_stream(Stream).
'$setup_wot'(chars(_Chars), Stream, _) :- !,
	charsio:open_mem_write_stream(Stream).
'$setup_wot'(chars(_Chars, _Tail), Stream, _) :- !,
	charsio:open_mem_write_stream(Stream).
'$setup_wot'(Stream, Stream, _) :-
	'$stream'(Stream), !.
'$setup_wot'(Output, _, Goal) :-
	'$do_error'(type_error(output,Output),Goal).

'$cleanup_wot'(Output, Stream, OldStream) :- !,
	'$cleanup_wot'(Output, Stream),
	set_output(OldStream).

'$cleanup_wot'(atom(Atom), Stream) :- !,
	charsio:peek_mem_write_stream(Stream, [], String),
	atom_codes(Atom, String),
	close(Stream).
'$cleanup_wot'(codes(Codes), Stream) :- !,
	charsio:peek_mem_write_stream(Stream, [], Codes),
	close(Stream).
'$cleanup_wot'(codes(Codes, Tail), Stream) :- !,
	charsio:peek_mem_write_stream(Stream, Tail, Codes),
	close(Stream).
'$cleanup_wot'(chars(Chars), Stream) :- !,
	charsio:peek_mem_write_stream(Stream, [], String),
	'$codes_to_chars'([], String, Chars),
	close(Stream).
'$cleanup_wot'(chars(Chars, Tail), Stream) :- !,
	charsio:peek_mem_write_stream(Stream, Tail, String),
	'$codes_to_chars'(Tail, String, Chars),
	close(Stream).
'$cleanup_wot'(_, _).

'$codes_to_chars'(String0, String, String0) :- String0 == String, !.
'$codes_to_chars'(String0, [Code|String], [Char|Chars]) :-
	atom_codes(Char, [Code]),
	'$codes_to_chars'(String0, String, Chars).










