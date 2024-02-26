/**
 * Language Server support
 *
 */

:- module(lsp, [
	      validate_file/2,
  pred_code/2
  ]).


:- use_module(library(lists)).
:- use_module(library(maplist)).
:- use_module(library(toks_lsp)).
:- reexport(library(python)).
:- use_module(library(yapi)).
:- use_module(library(hacks)).
:- use_module(library(completions)).

:- use_module(library(scanner)).

:- python_import(lsprotocol.types as t).
:- python_import(server).



%%
%% @pred pred_def(Ob,N0)
%%
%% find the definition for the predicate with name N0
%%
pred_code(Ob,N0) :-
    string_to_atom(N0, N),
    findall(P,name2symbol(N,P),Ps),
    	(var(Ob)
	->
	  Ob = Ps
	;
	  Ob.items := Ps
	).

user:pred_def(Ob, Name) :-
    pred_code(Ob,Name).

name2symbol(N,t(F,L,0,L1,0,L,0,L1,0)) :-
    writeln(N),
    current_predicate(N,Mod:G),
    writeln(G),
    functor(G,N,Ar),
    predicate_property(Mod:G,file(F) ),
    predicate_property(Mod:G,line_count(L)),
    L1 is L+1.

get_ref(N/A,M,Ref) :-
	scanner:use(predicate,N/A,M,_N0/_A0,_M0,File,L0-C0,LF-CF,LL0-LC0,LLF-LCF),
    atom_string(File,SFile),
    Ref = t(SFile,L0,C0,LF,CF,LL0,LC0,LLF,LCF).


%%
%% @pred pred_refs(URI,Line,Ch,Ob
%%
%% find the definition for the text at URI:Line:Ch
%%
user:pred_refs(Ob,URI,Line,Ch) :-
	string_concat(`file://`, FS, URI),
	string_to_atom(FS, Afs),
%	mkgraph(Afs),
	fail,name2symbol(Afs,Line,Ch,M:N/A),
	findall(Ref,get_ref(N/A,M,Ref),Refs),
%	writeln(go2t:Refs) ,
	(var(Ob)
	->
	  Ob = Refs
	;
	  Ob.items := Refs
	).


    user:complete(Line,Pos,Obj) :-
	completions(Line,Pos,L),
	Obj.items := L.

    user:add_dir(Self,URI):-
	string_concat(`file://`, FS, URI),
	atom_string(F,FS),
	file_directory_name(F,D),
	list_directory(D, Fs),
	maplist(add_file(Self, D), Fs).

%%
%% @pred validate_uri(Self,URI)
%%
%% check for errors or warnings in the file pointed to by URI. Obj is the
%% Python caller
%%
user:validate_uri(Self,URI) :-
    string_concat(`file://`,S,URI),
    atom_string(File,S),
    absolute_file_name(File,Path,[file_type(prolog)]),
    validate_file(Self,Path).


validate_file( Self,File) :-
    (
      predicate_property(user:portray_message(Sev,Msg),number_of_clauses(0))
      ->    
%      asserta((user:portray_message(Sev`xMsg) :- q_msg( Sev, File,Msg)),R),
      writeln(h),
      load_files(File,[ source_module(user)]),
      retractall(user:portray_message(_,_)),
   process_msgs(Self)
    ;    
      load_files(File,[source_module(user)])
).

user:validate_text(Self,URI,S) :-
   string_concat(`file://`,SFile,URI),
    atom_string(File, SFile),
    open(string(S),read,Stream,[file_name(File)]),
    (
      predicate_property(user:portray_message(Sev,Msg),number_of_clauses(0))
      ->    
      asserta((user:portray_message(Sev,Msg) :- q_msg( Sev,Msg)),R),
writeln(go),
      load_files(File,[ stream(Stream),source_module(user)]),
retractall(user:portray_message(_,_)),
    process_msgs(Self)
    ;    
      load_files(File,[ stream(Stream),source_module(user)])
).


q_msg(Sev, error(Err,Inf)) :-
    writeln(Sev),
    (Sev=warning;Sev=error),
    writeln(in),
    open(string(S),write,Stream,[alias(user_error)]),
    print_message(Sev,error(Err,Inf)),


    close(Stream),
    writeln(S),
    (
      yap_query_exception(parserLine, Inf, LN)
    ->
      true
    ;
      LN = 0),
    writeln(LN),
    (
	yap_query_exception(parserLinePos, Inf, Pos)
	->
	true
    ;
      Pos = 0
    ),
    writeln(Pos),
    recordz(msg,t(S,LN,Pos),_).

process_msgs(Self) :-
    findall(M, process_msg(M),Ms),
    (
      var(Self)
      ->
      Self=Ms
    ;
      Self:=Ms
    ).

process_msg(t(S,LN,Pos)) :-
    recorded(msg,t(S,LN,Pos),R),
    erase(R).

add_file(Self, D, File) :-
    absolute_file_name(File, Path,
			   [ file_type(prolog),
relative_to(D),
			     access(read),
			     file_errors(fail)
			     ]),
    once((
	   user:prolog_file_type(Suffix,prolog),
	   atom_concat(_, Suffix , Path)
	 )),
    !,
    validate_file(Self,Path).
add_file(_,_,_).

user:highlight_uri(Self, URI, Text):-
	string_concat(`file://`,S,URI),
	atom_string(File,S),
	open(string(Text),read,Stream,[alias(File)]),
	set_stream(Stream,file_name(File)),
    highlight_and_convert_stream(Self,Stream).

highlight_file(Self, File) :-
    open(File,read,Stream,[alias(File)]),
    highlight_and_convert_stream(Self,Stream).

user:highlight_text(Text,Self):-
    open(string(Text),read,Stream,[alias(data)]),
    highlight_and_convert_stream(Self, Stream).

highlight_and_convert_stream(Self,Stream) :-
    scan_stream(Stream,Ts),
    close(Stream),
    symbols(Ts,LTsf),
    (var(Self)
    ->
      Self = LTsf
    ;
%:= print(LTsf).
      Self.data := LTsf
    ).
