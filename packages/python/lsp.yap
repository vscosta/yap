/**
 * Language Server support
 *
 */

:- module(lsp, [
	      validate_file/2,
	      scan_file/2
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

user:open_uri(URI):-
    string_concat(`file://`,S,URI),
    atom_string(File,S),
    catch(    mkgraph(File), _Error, error_handler).
%lis`ting(user_error, scanner:def).

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
%% @pred pred_def(URI,Line,Ch,Ob
%%
%% find the definition for the text at URI:Line:Ch
%% 
    user:pred_def(URI,Line,Ch,Ob) :-
	string_concat(`file://`, FS, URI),
	string_to_atom(FS, Afs),
	name2symbol(Afs,Line,Ch,Mod:N0/Ar0),
	findall(P,symbol(N0/Ar0, Mod,P),Ps),
	(var(Ob)
	->
	  Ob = P
	;
	  Ob.items := Ps
	).



get_ref(N/A,M,Ref) :-
	scanner:use(predicate,N/A,M,_N0/_A0,_M0,File,L0-C0,LF-CF,LL0-LC0,LLF-LCF),     
    atom_string(File,SFile),
    Ref = t(SFile,L0,C0,LF,CF,LL0,LC0,LLF,LCF).


%%
%% @pred pred_refs(URI,Line,Ch,Ob
%%
%% find the definition for the text at URI:Line:Ch
%% 
    user:pred_refs(URI,Line,Ch,Ob) :-
	string_concat(`file://`, FS, URI),
	string_to_atom(FS, Afs),
	mkgraph(Afs),
	name2symbol(Afs,Line,Ch,M:N/A),
	findall(Ref,get_ref(N/A,M,Ref),Refs),
	writeln(go2t:Refs) ,
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
%% @pred validate_uri(URI,Obj)
%%
%% check for errors or warnings in the file pointed to by URI. Obj is the
%% Python caller
   %%
    user:validate_uri(URI,Obj):-
	string_concat(`file://`,S,URI),
	atom_string(File,S),
	absolute_file_name(File,Path,[file_type(prolog)]),
	validate_file(Obj,Path).

validate_text(S,FileName,Obj) :-
    open(string(S), read, Stream, [file_name(FileName),alias(data)]),
    validate_stream(Stream,Obj).

validate_stream(Stream,Self) :-
    self := Self,
    assert((user:portray_message(Sev,Msg) :- q_msg(Sev, Msg)),Ref),
    init_scanner_helper(Loop),
    ignore( load_files(data,[stream(Stream)]) ),
    close_scanner_helper(Loop),
    erase(Ref),
    findall(M,retract(msg(M)),L),
    Self.errors := L.

validate_file(Self, File) :-
    self := Self,
    assert((user:portray_message(Sev,Msg) :- q_msg(Sev, Msg)),Ref), 
    init_scanner_helper(Loop),
    compile(File),    
    close_scanner_helper(Loop),
    erase(Ref),
    findall(M,retract(msg(M)),L),
    (var(Self) -> writeln(L) ; Self.errors := L).

q_msg(Sev, error(Err,Inf)) :-
    Err =.. [_F|As],
    yap_error_descriptor(Inf, Desc),
    writeln(Desc),
    yap_query_exception(parserLine, Inf, LN),
    nonvar(LN),
    LN1 is LN-1,
    yap_query_exception(parserPos, Inf, Pos),
    q_msgs(As,Sev,S),
    writeln(user_error,Err),
    assert(msg(t(S,LN1,Pos))).
     

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

user:scan_uri(URI,Self):-
	string_concat(`file://`,S,URI),   
	atom_string(File,S),
	open(File,read,_,[alias(data)]),
	scan_and_convert_stream(Self).

scan_file(File,Self) :-
    open(File,read,_,[alias(data)]),
    scan_and_convert_stream(Self).

user:scan_text(Text,Self):-
	    open(string(Text),read,_,[alias(data)]),
	    scan_and_convert_stream(Self).

scan_and_convert_stream(Self) :-
    scan_stream(data, Ts),
    close(data),
    symbols(Ts,LTsf),
    (var(Self)
    ->
      Self = LTsf
    ;
      Self == show
    ->
      foldl(showt,Ts,LTsf   , _)
    ;	
      Self.data := (LTsf)
    ).

