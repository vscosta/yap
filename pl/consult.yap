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
	'$current_module'(M),
	'$ensure_loaded'(V).

'$ensure_loaded'(V) :- var(V), !,
	'$do_error'(instantiation_error,ensure_loaded(V)).
'$ensure_loaded'([]) :- !.
'$ensure_loaded'([F|Fs]) :- !,
	'$ensure_loaded'(F),
	'$ensure_loaded'(Fs).
'$ensure_loaded'(M:X) :- atom(M), !,
        '$current_module'(M0),
        '$change_module'(M),
        '$ensure_loaded'(X),
        '$change_module'(M0).
'$ensure_loaded'(X) :-
	'$find_in_path'(X,Y,ensure_loaded(X)),
	'$open'(Y, '$csult', Stream, 0), !,
        '$current_module'(M),
	( '$loaded'(Stream, M, TFN) ->
	    (  	recorded('$module','$module'(TFN,NM,P),_) ->
		'$import'(P,NM,M)
		;
		true
	    )
	;
	    '$reconsult'(X,M,Stream)
	    ),
	'$close'(Stream).
'$ensure_loaded'(X) :-		
	'$do_error'(permission_error(input,stream,X),ensure_loaded(X)).


compile(P) :-
	'$has_yap_or',
	'$do_error'(context_error(compile(P),clause),query).
compile(P) :-
	'$compile'(P).

% leave compile mode to 1 for native code.
'$compile'(M:A) :- !,
        '$current_module'(M0),
        '$change_module'(M),
        '$compile'(A),
        '$change_module'(M0).
'$compile'(A) :-
	'$compile_mode'(Old,0),
	'$reconsult'(A),
	'$compile_mode'(_,Old).

consult(Fs) :-
	'$has_yap_or',
	'$do_error'(context_error(consult(Fs),clause),query).
consult(Fs) :-
	'$consult'(Fs).

reconsult(Fs) :-
	'$has_yap_or', fail,
	'$do_error'(context_error(reconsult(Fs),clause),query).
reconsult(Fs) :-
	'$reconsult'(Fs).

'$reconsult'(V) :- var(V), !,
	'$do_error'(instantiation_error,reconsult(V)).
'$reconsult'([]) :- !.
'$reconsult'(M:X) :- atom(M), !,
        '$current_module'(M0),
        '$change_module'(M),
        '$reconsult'(X),
        '$change_module'(M0).
'$reconsult'([F|Fs]) :- !,
	'$reconsult'(F),
	'$reconsult'(Fs).
'$reconsult'(X) :-
	'$find_in_path'(X,Y,reconsult(X)),
	'$open'(Y,'$csult',Stream,0), !,
        '$current_module'(M),
	'$reconsult'(X,M,Stream),
	'$close'(Stream).
'$reconsult'(X) :-
	'$do_error'(permission_error(input,stream,X),reconsult(X)).

'$reconsult'(F,M,Stream) :-
	'$record_loaded'(Stream, M),
	fail.
'$reconsult'(F, OldModule, Stream) :-
	'$getcwd'(OldD),
	get_value('$consulting_file',OldF),
	'$set_consulting_file'(Stream),
	H0 is heapused, '$cputime'(T0,_),
	current_stream(File,_,Stream),
	get_value('$consulting',Old),
	set_value('$consulting',false),
	'$start_reconsulting'(F),
	'$start_consult'(reconsult,File,LC),
	'$remove_multifile_clauses'(File),
	recorda('$initialisation','$',_),
	'$print_message'(informational, loading(reconsulting, File)),
	'$loop'(Stream,reconsult),
	'$end_consult',
	'$clear_reconsulting',
	'$add_multifile_clauses'(File),
	set_value('$consulting',Old),
	set_value('$consulting_file',OldF),
	'$cd'(OldD),
	'$exec_initialisation_goals',
	'$current_module'(Mod,OldModule),
	( LC == 0 -> prompt(_,'   |: ') ; true),
	H is heapused-H0, '$cputime'(TF,_), T is TF-T0,
	'$print_message'(informational, loaded(reconsulted, File, Mod, T, H)),
	!.

'$start_reconsulting'(F) :-
	recorda('$reconsulted','$',_),
	recorda('$reconsulting',F,_).

'$initialization'(V) :-
	var(V), !,
	'$do_error'(instantiation_error,initialization(V)).
'$initialization'(C) :- number(C), !,
	'$do_error'(type_error(callable,C),initialization(C)).
'$initialization'(C) :- db_reference(C), !,
	'$do_error'(type_error(callable,C),initialization(C)).
'$initialization'(G) :-
	recorda('$initialisation',G,_),
	fail.
'$initialization'(_).


'$include'(V, _) :- var(V), !,
	'$do_error'(instantiation_error,include(V)).
'$include'([], _) :- !.
'$include'([F|Fs], Status) :- !,
	'$include'(F, Status),
	'$include'(Fs, Status).
'$include'(X, Status) :-
	'$find_in_path'(X,Y,include(X)),
	'$values'('$included_file',OY,Y),
	( '$open'(Y,'$csult',Stream,0), !,
		'$loop'(Stream,Status), '$close'(Stream)
	;
		'$do_error'(permission_error(input,stream,Y),include(X))
	),
	set_value('$included_file',OY).

'$do_startup_reconsult'(X) :-
	( '$access_yap_flags'(15, 0) ->
	  true
	;
	  set_value('$verbose',off)
	),
	( '$find_in_path'(X,Y,reconsult(X)),
	  '$open'(Y,'$csult',Stream,0) ->
		( '$access_yap_flags'(15, 0) -> true ; '$skip_unix_comments'(Stream) ),
		'$reconsult'(X,Stream), '$close'(Stream)
	;
		'$output_error_message'(permission_error(input,stream,X),reconsult(X))
	),
	( '$access_yap_flags'(15, 0) -> true ; halt).

'$skip_unix_comments'(Stream) :-
	'$peek'(Stream, 0'#), !, % 35 is ASCII for #
	'$get0_line_codes'(Stream, _),
	'$skip_unix_comments'(Stream).
'$skip_unix_comments'(_).


prolog_load_context(_, _) :-
	get_value('$consulting_file',[]), !, fail.
prolog_load_context(directory, DirName) :- 
	get_value('$consulting_file',FileName),
	(FileName = user_input ->
	  '$getcwd'(S),
	  atom_codes(DirName,S)
        ;
	  atom_codes(FileName,S),
	  '$strip_file_for_scd'(S,Dir,Unsure,Unsure),
	  atom_codes(DirName,Dir)
	).
prolog_load_context(file, FileName) :- 
	get_value('$included_file',IncFileName),
	( IncFileName = [] ->
	  get_value('$consulting_file',FileName)
        ;
           FileName = IncFileName
        ).
prolog_load_context(module, X) :-
	'$current_module'(X).
prolog_load_context(source, FileName) :-
	get_value('$consulting_file',FileName).
prolog_load_context(stream, Stream) :- 
	'$fetch_stream_alias'('$loop_stream', Stream).
prolog_load_context(term_position, Position) :- 
	'$fetch_stream_alias'('$loop_stream', Stream),
	stream_position(Stream, Position).


'$loaded'(Stream,M,F1) :-
	'$file_name'(Stream,F),
	'$loaded_file'(F,M,F1).

% if the file exports a module, then we can
% be imported from any module.
'$loaded_file'(F,M,F1) :-
	recorded('$module','$module'(F1,_,P),_),
	recorded('$loaded','$loaded'(F1,_,Age),R),
	'$same_file'(F1,F), !,
	'$loaded_file_age'(F, R).
'$loaded_file'(F,M,F1) :-
	recorded('$loaded','$loaded'(F1,M,Age),R),
	'$same_file'(F1,F), !,
	'$loaded_file_age'(F, R).

'$loaded_file_age'(F, R) :-
        '$file_age'(F,CurrentAge),
         ((CurrentAge = Age ; Age = -1)  -> true; erase(R), fail).



path(Path) :- findall(X,'$in_path'(X),Path).

'$in_path'(X) :- recorded('$path',Path,_),
		atom_codes(Path,S),
		( S = ""  -> X = '.' ;
		  atom_codes(X,S) ).

add_to_path(New) :- add_to_path(New,last).

add_to_path(New,Pos) :-
	atom(New), !,
	'$check_path'(New,Str),
	atom_codes(Path,Str),
	'$add_to_path'(Path,Pos).

'$add_to_path'(New,_) :- recorded('$path',New,R), erase(R), fail.
'$add_to_path'(New,last) :- !, recordz('$path',New,_).
'$add_to_path'(New,first) :- recorda('$path',New,_).

remove_from_path(New) :- '$check_path'(New,Path),
			recorded('$path',Path,R), erase(R).

'$check_path'(At,SAt) :- atom(At), !, atom_codes(At,S), '$check_path'(S,SAt).
'$check_path'([],[]).
'$check_path'([Ch],[Ch]) :- '$dir_separator'(Ch), !.
'$check_path'([Ch],[Ch,A]) :- !, integer(Ch), '$dir_separator'(A).
'$check_path'([N|S],[N|SN]) :- integer(N), '$check_path'(S,SN).

% add_multifile_predicate when we start consul
'$add_multifile'(Name,Arity,Module) :-
	get_value('$consulting_file',File),
	'$add_multifile'(File,Name,Arity,Module).

'$add_multifile'(File,Name,Arity,Module) :-
	recordzifnot('$multifile_defs','$defined'(File,Name,Arity,Module),_), !,
	fail.
'$add_multifile'(File,Name,Arity,Module) :-
	recorded('$mf','$mf_clause'(File,Name,Arity,Module,Ref),R),
	erase(R),
	erase(Ref),
	fail.
'$add_multifile'(_,_,_,_).

% retract old multifile clauses for current file.
'$remove_multifile_clauses'(FileName) :-
	recorded('$multifile_defs','$defined'(FileName,_,_,_),R1),
	erase(R1),
	fail.
'$remove_multifile_clauses'(FileName) :-
	recorded('$mf','$mf_clause'(FileName,_,_,Module,Ref),R),
	'$erase_clause'(Ref, Module),
	erase(R),
	fail.
'$remove_multifile_clauses'(_).

