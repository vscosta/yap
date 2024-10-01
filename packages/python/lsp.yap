/*
 * Language Server support
 *
 */

:- module(lsp, [
	      validate_file/2,
highlight_file/2
  ]).


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
    atom_string( Name, S),
    current_module( Mod),
    current_predicate(Mod:Name/Ar),
    predicate_property(Mod:G,file(F) ),
    predicate_property(Mod:G,line_count(L)),
    writeln(user_error,(F+L)),
    string_atom( SMod, Mod),
    Ob.defs.append(t(F,L,0,SMod,Ar)),
    fail.
user:pred_def(_Ob, _Name).

name2symbol(Name,t(F,L1,0)) :-
    strip_module(Name,Mod,N),
    current_predicate(N,Mod:G),
    functor(G,N,_Ar),
    predicate_property(Mod:G,file(F) ),
    predicate_property(Mod:G,line_count(L)).

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


user:complete(Self,Prefix,_Pos) :-
	completions(Prefix,_,FCs),
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
%% Python caller
%%
user:validate_uri(Self,URI) :-
    string_concat(`file://`,S,URI),
    atom_string(File,S),
    absolute_file_name(File,Path,[file_type(prolog)]),
    validate_file(Self,Path).

:- dynamic lsp_on/0.

validate_file( Self,File) :-
   assert(lsp_on),
   load_files(File,[ if(true),def_use_map(true)]),
   forall(retract(msg(T)),Self.errors.append(T)),
   retract(lsp_on).

user:validate_text(Self,URI,S) :-
    assert(lsp_on),
    string_concat(`file://`,SFile,URI),
    atom_string(File, SFile),
    open(string(S),read,Stream,[alias(File)]),
    set_stream(Stream,file_name(File)),
    catch(load_files(File,[ stream(Stream), if(true),dry_run(true)]),E,writeln(user_error,E)),
    findall(TERR,recorded(msg,TERR,_Ri),Ts),
    eraseall(msg),
    retractall(lsp_on),
    (var(Self) -> Self=Ts;Self.errors := Ts),
    !.

user:portray_message(A,B):-
    writeln(user_error,file=A:B),
    lsp_on,
    q_msg(A,B).

q_info(Exc, LN, Pos, Size, ErrN, ErrT) :-
   (
      exception_property(`parserLine` , Inf, LN0)
    ->
	  LN is max(LN0,0)
    ;
    LN = 1),
    (
	exception_property(`errorNo` , Inf, ErrN)
    ->
	  true
    ;
    ErrN = -1),
    (
      exception_property(`errorAsText` , Inf, ErrA )
    ->
	  atom_string(ErrA,ErrT)
    ;
    ErrT = `unknown` ),
    (
	exception_property(`parserLinePos`, Inf, Pos0)
	->
	Pos is max(Pos0,0)
    ;
    Pos =0 ),
    
    (
	exception_property(`parserSize` , Inf, Size0)
    ->
	  Size is max(Size0,0)
    ;
    Size = 0),
(
	exception_property(`ErrorMsg`, Inf, ErrorMsg)
	->
	true
    ;
	  ErrorMsg = `ugh`
    ),
writeln(user_error,Pos),
    ( Sev == error
  ->
      Sv = `error` 
  ;
Sv = `warning`
),
    Err =..LErr,
    q_msgs(LErr,ErrorMsg,Sev,S),

q_msg(_warning, error( style_check(singletons(singletons,[VName,LN,Pos|_Culprit],_), Exc)) :-
    lsp_on,
    !,
atom_length(VName,Size),
    format(string(S),'~a is a singleton variable',[VName]),
   recordz(msg,t(`warning`,0,` `,S,LN,Size,Pos),_).
q_msg(Sev, error(Err,Inf)) :-
q_info(Exc, LN, Pos, Size, ErrN, ErrT),
    lsp_on,
     recordz(msg,t(Sv,ErrN,ErrT,S,LN,Size,Pos),_).


q_msgs([A1], Extra, N,S) :-
    format(string(S),'~s: ~w.~n~s',[N,A1,Extra]).
q_msgs([A1,A2], Extra, N,S) :-
    format(string(S),'~s: ~w ~w.~n~s',[N,A1,A2,Extra]).
q_msgs([A1,A2,A3], Extra, N,S) :-
    format(string(S),'~s: ~w ~w ~w.~n~s',[N,A1,A2,A3,Extra]).


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
    writeln(Ts),
    symbols(Ts,LTsf),
    (var(Self)
    ->
      Self = LTsf
    ;
%:= print(LTsf).
      Self.data := LTsf
    ).

:- writeln(ok).
