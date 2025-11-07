/*
 * Language Server support
 *
 */

:- module(lsp, [
	      validate_file/2,
highlight_file/2
  ]).

:- set_prolog_flag(double_quotes, string).


:- dynamic my/2.

:- use_module(library(lists)).
:- use_module(library(maplist)).
:- use_module(library(toks_lsp)).
:- reexport(library(python)).
:- use_module(library(yapi)).
:- use_module(library(hacks)).
:- use_module(library(completions)).

:- use_module(library(scanner)).

%:- python_import(lsprotocol.types as t).
%:- python_import(pygls.server).


name2symbol(File,UL0,U,Mod:N0/Ar0):-
	scanner:use(predicate,N0/Ar0,Mod,_NAr,_MI,File,UL0-UC0,UL0-UCF,_S1,_E1),
    UC0=<U,
    U=<UCF,
    !.

name2symbol(File,UL0,U,Mod:N0/Ar0):-
%listing(scanner:use),
	scanner:def(predicate,N0/Ar0,Mod,File,UL0-UC0,UL0-UCF,_S1,_E1),
	UC0=<U,
	U=<UCF,
	!.


symbol(N0/Ar0,Mod,
	t(SFile,L0,C0,LF,CF,LL0,LC0,LLF,LCF)) :-
    (
 	 scanner:def(predicate,N0/Ar0,Mod,DFile,L0-C0,LF-CF,LL0-LC0,LLF-LCF)
      *->
      true
    ;
      functor(G0,N0,Ar0),
      predicate_property(Mod:G0,file_name(DFile)),
      predicate_property(Mod:G0,line_number(L0)),
      C0=0,
      LF=L0,
      atom_length(N0,CF),
      LL0=L0,
      LC0= 0,
      LLF is L0+2,
      LCF= 0
    ),
    atom_string(DFile,SFile).

%%
%% @pred pred_def(Ob,N0)
%%
%% find the definition for the text at URI:Line:Ch
%%
user:pred_def(Ob,URI,Line,Ch) :-
	string_concat(`file://`, FS, URI),
	string_to_atom(FS, Afs),
	name2symbol(Afs,Line,Ch,Mod:N0/Ar0),
	findall(P,symbol(N0/Ar0, Mod,P),Ps),
	(var(Ob)
	->
	  Ob = Ps
	;
	  Ob.items := Ps
	).

user:pred_def(Ob, S) :-
    writeln(user_error,(S)),
    atom_string( Name, S),
    current_module( Mod),
    current_predicate(Mod:Name/Ar),
    functor(G,Name,Ar),
    predicate_property(Mod:G,file(F) ),
    predicate_property(Mod:G,line_count(L)),
    writeln(user_error,(F+L)),
    string_atom( SMod, Mod),
    Ob.defs.append(t(F,L,0,SMod,Ar)),
    fail.
user:pred_def(_Ob, _Name).

name2symbol(Name,t(F,Lines,0)) :-
    strip_module(Name,Mod,N),
    current_predicate(N,Mod:G),
    functor(G,N,_Ar),
    predicate_property(Mod:G,file(F) ),
    predicate_property(Mod:G,line_count(Lines)).

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
	name2symbol(Afs,Line,Ch,M:N/A),
	findall(Ref,get_ref(N/A,M,Ref),Refs),
%	writeln(go2t:Refs) ,
	(var(Ob)
	->
	  Ob = Refs
	;
	  Ob.items := Refs
	).


user:complete(Self,_Line,_Pos,Prefix) :-
    completions(Prefix,FCs),
    ( var(Self)-> Self = FCs ; Self.items := FCs ).

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
%% absolute_file_name(File,Path,[file_type(prolog)]),
%%    validate_file(Self,Path).

:- dynamic lsp_on/0.



validate_file( Self,File) :-
    assert(lsp_on),
    absolute_file_name(File, Path,
		       [ file_type(prolog),
			 access(read),
			 file_errors(fail)
		       ]),
    atom_string(Path, SPath),
    string_concat("file://",SPath,URI),
    asserta(my(Self,URI)),
    load_files(Path,[]),
    retract(my(Self,URI)),
   retract(lsp_on).


user:validate_text(Self,URI,S) :-
    writeln(rs),
	 string_concat("file://",SFile,URI),
	 atom_string(File, SFile),
	 	 writeln(File),
    open(string(S),read,Stream,[alias(File)]),
	 	 writeln(done),
    set_stream(Stream,file_name(File)),
    assert(lsp_on),
    asserta(my(Self,URI)),
    load_files(File,[stream(Stream)]),
%    forall(my(URI,Err),Self[URI]:=Err),
    retract(my(Self,URI)),
    retract(lsp_on).


q_msg(informational, _, _, _,  _) :-
    !,
    fail.
q_msg(help, _, _, _,  _) :-
    !,
    fail.
q_msg(warning, error(style_check(singletons,[VName,Line,Column,_F0],_P),_),S,Line,Column) :-
    !,
    format(string(S), 'singleton variable ~s.~n ', [VName]).
q_msg(warning, error(style_check(multiple,_,_I ) ,Desc ), S, L0,C0) :-
    !,
    exception_property(parserLine, Desc, L0),
    exception_property(parserLinePos, Desc, C0),
    format(string(S), 'previously defined.~n',[]).
q_msg(warning, error(style_check(discontiguous,_,_I ), Desc), S, L0,C0) :-
    !,
    exception_property(parserLine, Desc, L0),
    exception_property(parserLinePos, Desc, C0),
    S = "discontiguous.~n".
q_msg(_error, error(syntax_error(_Msg), Desc), "syntax error",L0,C0) :-
    !,	    
    exception_property(parserLine, Desc, L0),
    exception_property(parserLinePos, Desc, C0).


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

user:highlight_text(Self,Text):-
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

user:portray_message(A,B):-
    writeln(B),
    my(Self,URI),
    q_msg(A,B,S,Line,Column),
    !,
    (var(Self)
    ->
	writeln(URI/t(S,Line,Column))
    ;
    % assertz(lsp(URI,t(A,S,Line,Column)),
    Self.errors[URI].append(t(A,S,Line,Column))
    ),
    !,			     
    fail.

:- writeln(ok).
