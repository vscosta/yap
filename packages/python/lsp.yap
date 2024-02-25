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
    asserta((user:portray_message(Sev,Msg) :- q_msg(Sev, Msg),fail),Ref),
    load_files(File,[]),
    erase(Ref),
    forall(retract(msg(T)),Self.errors.append(T)).

user:validate_text(Self,URI,S) :-
   string_concat(`file://`,SFile,URI),
    atom_string(File, SFile),
    open(string(S),read,Stream,[file_name(File)]),  
    asserta((user:portray_message(Sev,Msg) :- q_msg(Sev, Msg), fail),R),
    start_low_level_trace,
    load_files(Stream,[ stream(Stream)]),
    findall(T,(recorded(msg,T,R),erase(R)),Ts),
   erase(R),
    Self.errors := Ts,
    fail.



q_msg(Sev, error(Err,Inf)) :-
    Err =.. [_F|As],
    yap_error_descriptor(Inf, Desc),
    (
	yap_query_exception(parserLine, Desc, LN)
    ->
    true
    ;
    LN = 0),
    (
	yap_query_exception(parserPos, Desc, Pos)
	->
	true
    ;
    Pos =0 ),
    q_msgs(As,Sev,S),
    recordz(msg,t(S,LN,Pos),_).


q_msgs([], N,S) :-
    format(string(S ),'~s.',[N]).
q_msgs([A1], N, S) :-
    format(string(S),'~s: ~w.',[N,A1]).
q_msgs([A1,A2], N, S) :-
    format(string(S),'~s: ~w ~w.',[N,A1,A2]).
q_msgs([A1,A2,A3], N, S) :-
    format(string(S),'~s: ~w ~w ~w.',[N,A1,A2,A3]).


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
