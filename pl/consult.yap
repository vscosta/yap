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
* File:		consult.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	Consulting Files in YAP					 *
*									 *
*************************************************************************/

ensure_loaded(V) :-
	'$ensure_loaded'(V).

'$ensure_loaded'(V) :- var(V), !,
	throw(error(instantiation_error,ensure_loaded(V))).
'$ensure_loaded'([]) :- !.
'$ensure_loaded'([F|Fs]) :- !,
	'$ensure_loaded'(F),
	'$ensure_loaded'(Fs).
'$ensure_loaded'(M:X) :- atom(M), !,
        '$current_module'(M0),
        '$change_module'(M),
        '$ensure_loaded'(X),
        '$change_module'(M0).
'$ensure_loaded'(X) :- atom(X), !,
	'$find_in_path'(X,Y),
	( '$open'(Y, '$csult', Stream, 0), !,
	     ( '$loaded'(Stream) ->
		(  '$consulting_file_name'(Stream,TFN),
		    '$recorded'('$module','$module'(TFN,M,P),_) ->
			'$current_module'(T), '$import'(P,M,T)
		;
		   true
		)
	     ;
	       '$record_loaded'(Stream),
	       '$reconsult'(X,Stream)
	     ),
	     '$close'(Stream)
	;
		
	throw(error(permission_error(input,stream,X),ensure_loaded(X)))
	).
'$ensure_loaded'(library(X)) :- !,
	'$find_in_path'(library(X),Y),
	( '$open'(Y,'$csult',Stream, 0), !,
	     ( '$loaded'(Stream) ->
		(  '$consulting_file_name'(Stream,TFN),
		    '$recorded'('$module','$module'(TFN,M,P),_) ->
			'$current_module'(T), '$import'(P,M,T)
		;
		   true
		)
	     ;
	       '$record_loaded'(Stream),
	       '$reconsult'(Y,Stream)
	     ),
	     '$close'(Stream)
	;
	throw(error(permission_error(input,stream,library(X)),ensure_loaded(library(X))))
	).
'$ensure_loaded'(V) :- 
	throw(error(type_error(atom,V),ensure_loaded(V))).


compile(P) :-
	'$has_yap_or',
	throw(error(context_error(compile(P),clause),query)).
compile(P) :-
	'$compile'(P).

% leave compile mode to 1 for native code.
'$compile'(A) :-
	'$compile_mode'(Old,0),
	'$reconsult'(A),
	'$compile_mode'(_,Old).

consult(Fs) :-
	'$has_yap_or',
	throw(error(context_error(consult(Fs),clause),query)).
consult(Fs) :-
	'$consult'(Fs).

reconsult(Fs) :-
	'$has_yap_or', fail,
	throw(error(context_error(reconsult(Fs),clause),query)).
reconsult(Fs) :-
	'$reconsult'(Fs).

'$reconsult'(V) :- var(V), !,
	throw(error(instantiation_error,reconsult(V))).
'$reconsult'([]) :- !.
'$reconsult'(M:X) :- atom(M), !,
        '$current_module'(M0),
        '$change_module'(M),
        '$reconsult'(X),
        '$change_module'(M0).
'$reconsult'([F|Fs]) :- !,
	'$reconsult'(F),
	'$reconsult'(Fs).
'$reconsult'(X) :- atom(X), !,
	'$find_in_path'(X,Y),
	( '$open'(Y,'$csult',Stream,0), !,
		'$record_loaded'(Stream),
		'$reconsult'(X,Stream), '$close'(Stream)
	;
		throw(error(permission_error(input,stream,X),reconsult(X)))
	).
'$reconsult'(library(X)) :- !,
	'$find_in_path'(library(X),Y),
	( '$open'(Y,'$csult',Stream,0), !,
		'$record_loaded'(Stream),
		'$reconsult'(library(X),Stream), '$close'(Stream)
	;
		throw(error(permission_error(input,stream,library(X)),reconsult(library(X))))
	).
'$reconsult'(V) :- 
	throw(error(type_error(atom,V),reconsult(V))).

'$reconsult'(F,Stream) :-
	'$getcwd'(OldD),
	'$get_value'('$consulting_file',OldF),
	'$set_consulting_file'(Stream),
	H0 is heapused, '$cputime'(T0,_),
	current_stream(File,_,Stream),
	'$get_value'('$consulting',Old),
	'$set_value'('$consulting',false),
	'$current_module'(OldModule),
	'$start_reconsulting'(F),
	'$start_consult'(reconsult,File,LC),
	'$recorda'('$initialisation','$',_),
	'$print_message'(informational, loading(reconsulting, File)),
	'$loop'(Stream,reconsult),
	'$exec_initialisation_goals',
	'$current_module'(Mod,OldModule),
	'$end_consult',
	'$clear_reconsulting',
	( LC == 0 -> prompt(_,'   |: ') ; true),
	H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
	'$print_message'(informational, loaded(reconsulted, File, Mod, T, H)),
	'$set_value'('$consulting',Old),
	'$set_value'('$consulting_file',OldF),
	'$cd'(OldD),
	!.

'$start_reconsulting'(F) :-
	'$recorda'('$reconsulted','$',_),
	'$recorda'('$reconsulting',F,_).

'EMACS_FILE'(F,File0) :-
	'$format'('''EMACS_RECONSULT''(~w).~n',[File0]),
	'$getcwd'(OldD),
	'$open'(F,'$csult',Stream,0),
	'$find_in_path'(File0,File),
	'$open'(File,'$csult',Stream0,0),
	'$get_value'('$consulting_file',OldF),
	'$set_consulting_file'(Stream0),
	H0 is heapused, '$cputime'(T0,_),
	'$get_value'('$consulting',Old),
	'$set_value'('$consulting',false),
	'$start_reconsulting'(File),
	'$start_consult'(reconsult,File,LC),
	'$current_module'(OldModule),
	'$recorda'('$initialisation','$',_),
	'$print_message'(informational, loading(reconsulting, File)),
	'$loop'(Stream,reconsult),
	'$exec_initialisation_goals',
	'$current_module'(Mod,OldModule),
	'$end_consult',
	'$clear_reconsulting',
	( LC == 0 -> prompt(_,'   |: ') ; true),
	H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
	'$print_message'(informational, loaded(reconsulted, File, Mod, T, H)),
	'$set_value'('$consulting',Old),
	'$set_value'('$consulting_file',OldF),
	'$cd'(OldD),
	!.


'$initialization'(V) :-
	var(V), !,
	throw(error(instantiation_error,initialization(V))).
'$initialization'(C) :- number(C), !,
	throw(error(type_error(callable,C),initialization(C))).
'$initialization'(C) :- db_reference(C), !,
	throw(error(type_error(callable,C),initialization(C))).
'$initialization'(G) :-
	'$recorda'('$initialisation',G,_),
	fail.
'$initialization'(_).


'$include'(V, _) :- var(V), !,
	throw(error(instantiation_error,include(V))).
'$include'([], _) :- !.
'$include'([F|Fs], Status) :- !,
	'$include'(F, Status),
	'$include'(Fs, Status).
'$include'(X, Status) :- atom(X), !,
	'$find_in_path'(X,Y),
	'$values'('$included_file',OY,Y),
	( '$open'(Y,'$csult',Stream,0), !,
		'$loop'(Stream,Status), '$close'(Stream)
	;
		throw(error(permission_error(input,stream,Y),include(X)))
	),
	'$set_value'('$included_file',OY).
'$include'(V, _) :- 
	throw(error(type_error(atom,V),include(V))).

'$do_startup_reconsult'(X) :-
	( '$access_yap_flags'(15, 0) ->
	  true
	;
	  '$set_value'('$verbose',off)
	),
	'$find_in_path'(X,Y),
	( '$open'(Y,'$csult',Stream,0), !,
		'$record_loaded'(Stream),
		( '$access_yap_flags'(15, 0) -> true ; '$skip_unix_comments'(Stream) ),
		'$reconsult'(X,Stream), '$close'(Stream)
	;
		'$output_error_message'(permission_error(input,stream,X),reconsult(X))
	),
	( '$access_yap_flags'(15, 0) -> true ; halt).

'$skip_unix_comments'(Stream) :-
	'$peek_byte'(Stream, 0'#), !, % 35 is ASCII for #
	'$get0_line_codes'(Stream, _),
	'$skip_unix_comments'(Stream).
'$skip_unix_comments'(_).


prolog_load_context(_, _) :-
	'$get_value'('$consulting_file',[]), !, fail.
prolog_load_context(directory, DirName) :- 
	'$get_value'('$consulting_file',FileName),
	(FileName = user_input ->
	  '$getcwd'(S),
	  atom_codes(DirName,S)
        ;
	  atom_codes(FileName,S),
	  '$strip_file_for_scd'(S,Dir,Unsure,Unsure),
	  atom_codes(DirName,Dir)
	).
prolog_load_context(file, FileName) :- 
	'$get_value'('$included_file',IncFileName),
	( IncFileName = [] ->
	  '$get_value'('$consulting_file',FileName)
        ;
           FileName = IncFileName
        ).
prolog_load_context(module, X) :-
	'$current_module'(X).
prolog_load_context(source, FileName) :-
	'$get_value'('$consulting_file',FileName).
prolog_load_context(stream, Stream) :- 
	'$fetch_stream_alias'('$loop_stream', Stream).
prolog_load_context(term_position, Position) :- 
	'$fetch_stream_alias'('$loop_stream', Stream),
	stream_position(Stream, Position).


'$loaded'(Stream) :-
	'$file_name'(Stream,F),			%
	'$recorded'('$loaded','$loaded'(F,Age),R), !,
        '$file_age'(F,CurrentAge),
         ((CurrentAge = Age ; Age = -1)  -> true; erase(R), fail).



