
/*
 * Language Server support
 *
 */

:- module(lsp, [
  validate_file/2,
  validate_text/3,
  validate_source/3,
  symbol_at/3,
  definition/3,
  refs/3,
  file_symbols/2,
  workspace_symbols/1,
  analyse/5,
  complete/2]).

    /*
highlight_text/2,pred_def/4,
pred_def/2,
pred_refs/4,
highlight_file/2,
add_dir/2
  ]).
*/
    
:- set_prolog_flag(double_quotes, string).

:- dynamic state/2, lsp_on/0, use/9, def/6, dec/6.

:- use_module(library(lists)).
:- use_module(library(maplist)).
:- reexport(library(python)).
:- use_module(library(yapi)).
:- use_module(library(toks_lsp)).
:- use_module(library(hacks)).
:- use_module(library(completions)).

:- use_module(library(scanner)).

%:- python_import(lsprotocol.types as t).
%:- python_import(pygls.server).


    %%
    %% symbol(AllSymbowls)
    %%
uri_symbols(URI, All) :-
    string_concat("file://", FileS, URI),
    string_to_atom(FileS, File),
    findall(t(Name,Line), defa(Name,Line,File),All).

file_symbols(File, All) :-
   absolute_file_name(File, Path,
    [ file_type(prolog),
      access(read),
expand(true),
      file_errors(fail)
      ]),
    validate_file(Path,_),
   findall(t(Name,Line), defa(Name,Line,Path),All).

defa(Word,Line,File) :-
    def(Name,_,_,Line,File,_),
    atom_string(Name,Word).
    
workspace_symbols(All) :-
    findall(t(Name,Line,URI), symbol_in_def(Name,Line,URI), All).

symbol_in_def(Word,Line,URI) :-
    def(Name,_,_,Line,File,_),
    string_to_atom(Word,Name),
    string_to_atom(FileS, File),
    string_concat("file://", FileS, URI).

    %%
    %%
    
complete(Prefix, FCs) :-
    completions(Prefix,FCs),
    !.
    complete(_Prefix, []).

%%
%% @pred validate_uri(Self,URI)
%%
%% check for errors or warnings in the file pointed to by URI. Obj is the
%% absolute_file_name(File,Path,[file_type(prolog)]),
%%    validate_file(Self,Path).

:- dynamic lsp/1, m/1.

    /**
    read the line and store some info.
    */
user:term_expansion(G, GF) :-
    lsp(on),
    prolog_load_context(file, F ),
    prolog_load_context(term_position, Pos ),
    current_source_module(M, M ),
    arg(2, Pos, Line),
    analyse(G,Line,F,M,GF),
    !.

analyse((:- G), L, F, M, (:- true)) :-
 !,
 directive(G, L, F, M).
analyse((:- _G), _L, _F, _M, (:- true)) :-
  !.
analyse(Cl, L, F, M, g) :-
 rule(Cl, L, F, M).
analyse(_G, _L, _F, _M,  g).

rule((A0:- B),Line,File,Module) :-
    strip_module(Module:A0,MH,A),
    functor(A,Na,Ar),
    (    
    def(Na,Ar,MH,_,_,predicate)
       ->
    true
       ;
    assert(def(Na,Ar,MH,Line,File,predicate))
    ),
    body(B,Line,File,Module,MH:Na/Ar).
directive(module(M,Ls),L,F,M0) :-
    assert(def('',0,M,L,F,module)),
    assert(use('',0,M,'',0,M0,L,F,module)),
    current_source_module(M0,M),
    maplist(mod(L,F,M),Ls).
directive(op(A,B,C), _Line,_File,M) :- 
    op(A,B,M:C).
directive(use_module(_A), _Line,_File,_M) :- 
    !.
directive(use_module(_A,_B), _Line,_File,_M) :- 
    !.
directive(load_files(_A,_B), _Line,_File,_M) :- 
    !.
directive(ensure_loaded(_A), _Line,_File,_M) :- 
    !.
directive(consult(_A), _Line,_File,_M) :- 
    !.
directive(reconsult(_A), _Line,_File,_M) :- 
    !.
directive(compile(_A), _Line,_File,_M) :- 
    !.
directive(use_module(_A,_B,_C), _Line,_File,_M) :- 
    !.
directive((A,B), Line,File,M) :- 
    !,
    directive(A, Line,File,M),
    directive(B, Line,File,M).

body(A,_L,_F,_M,_) :-
    var(A),
!.
body(M:A,L,F,_M,P0) :-
    !,
    body(A,L,F,M,P0).
body((A,B),L,F,M,P0) :-
    !,
    body(A,L,F,M,P0),
    body(B,L,F,M,P0).
body((A;B),L,F,M,P0) :-
    !,
    body(A,L,F,M,P0),
    body(B,L,F,M,P0).
body((A->B),L,F,M,P0) :-
    !,
    body(A,L,F,M,P0),
    body(B,L,F,M,P0).
body(A,L,F,M,M0:Na0/Ar0) :-
    functor(A,NA,Ar),
    assert(use(NA,Ar,M,Na0,Ar0,M0,L,F,predicate)).


mod(Line,File,M,N/A) :- assert(dec(N,A,M,Line,File,export(predicate))).
mod(Line,File,M,N//A) :-
 A1 is A+ 2,
assert(dec(N,A1,M,Line,File,export(predicate))).
mod(Line,File,M, op(A,B,C)) :- 
    op(A,B,M:C),
 assert(dec(C,A,B,Line,File,export(op))).


validate_file(File, Errors) :-
%    atom_string(File, SFile),
    %    string_concat("file://",SFile,URI),
    current_source_module(DefaultModule,user),
    absolute_file_name(File, Path,
    [ file_type(prolog),
      access(read),
      expand(true),
      file_errors(fail)
      ]),
    validate_source(Path, DefaultModule, Errors).


validate_source(Path, DefaultModule, Errors) :-
    assert(lsp(on)),
    load_files(Path,[]),
    retractall(lsp(_)),
    findall(T,retract(m(T)),Errors),
    current_source_module(_,DefaultModule).
    
validate_text(URI,S,Ts) :-
      writeln(user_error, URI),
    string_concat("file://", FileAsS, URI),
    atom_string(FileAsS, File),
    retractall(def(_,_,_,_,File,_)),
    retractall(use(_,_,_,_,_,_,_,File,_)),
    retractall(dec(_,_,_,_,File,_)),
    open(string(S),read,Stream),
    current_source_module(DefaultModule,user),
    assert(lsp(on)),
    load_files(File,[stream(Stream)]),
    %close(Stream),
    current_source_module(_,DefaultModule),
    retractall(lsp(_)),
    findall(T,retract(m(T)),Ts).

q_msg(informational, _, _) :-
    !,
    fail.
q_msg(help, _, _) :-
    !,
    fail.
q_msg(warning, error(style_check(singletons,[VName,Line,Column,_F0],_),_Desc),t("warning",S, Line,Column, Line,EndCol)) :-
    !,
    format(string(S), 'singleton variable ~s.~n ', [VName]),
    atom_length(VName, Len),
    EndCol is Column+Len.
 q_msg(warning, error(style_check(multiple,[F0|L],I ) ,_Desc ), t("warning",S, L,Column,L,EndCol)) :-
    !,
    Column=0,
    format(string(S), '~w previously defined at ~s.~n',[I,F0]),
    I = _:Name/_,
    atom_length(Name, Len),
    EndCol is Column+Len.
q_msg(warning, error(style_check(discontiguous,_,_I ), _Desc), t("warning",S, L,Column, L, EndCol)) :-
    !,
    Column=0,
    format(string(S), 'discontiguous definition for ~w.~n',[I]),
    I = _:Name/_,
    atom_length(Name, Len),
    EndCol is Column+Len,
    S = "discontiguous.~n".
q_msg(_error, error(syntax_error(_Msg), Desc),  t("error","syntax error", L,LPos,L,LP1)) :-
    exception_property(parserLine, Desc, L),
    exception_property(parserLinePos, Desc, LPos),
    exception_property(parserSize, Desc, Size),
    !,
    LP1 is LPos+Size.
q_msg(_,Error,t("ignored",Msg,1,0,2,0)) :-
    term_to_string(Error,Msg).

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

    
symbol_at(Line, Pos, Word) :-
    open(string(Line),read,Stream,[alias(data)]),
    scan_stream(Stream,Ts),
    close(Stream),
    at(Ts, Pos, Symbol),
    !,
    atom_string(Symbol,Word).
    
at([t(atom(Name), _Line, PosS, Sz)|_], Pos, Name ) :-
    PosS =< Pos,
    PosS+Sz >  Pos,
    !.

    at([t(_, _Line, PosS, _Sz)|Toks], Pos, Name ) :-
    PosS <  Pos,
    !,
    at(Toks, Pos, Name ).

definition(Line,Pos,t(URI,L,1,Sz)) :-
    symbol_at(Line, Pos, Word),
    string_length(Word,Sz),
    atom_string(NA,Word),
    def(NA,_Ar,_MH,L,File,predicate),
    atom_string(File, FileS),
    string_concat("file://", FileS, URI).

    refs(Line,Pos,L) :-
        symbol_at(Line, Pos, Word),
    string_to_atom(Word, Na0),
    findall(t(URI,L),ref(Na0,URI,L), L).

    ref(Na0,URI,L) :-
    use(_NA,_Ar,_M,Na0,_Ar0,_M0,L,File,predicate),
    string_to_atom(FileS, File),
    string_concat("file://", FileS, URI).
   
 

    
user:highlight_uri( URI, Text, LTsf):-
	string_concat(`file://`,S,URI),
	atom_string(File,S),
	open(string(Text),read,Stream,[alias(File)]),
	set_stream(Stream,file_name(File)),
	highlight_and_convert_stream(Stream, LTsf).

highlight_file(Self, File) :-
    open(File,read,Stream,[alias(File)]),
    highlight_and_convert_stream(Self,Stream).

highlight_text(Text, LTsf):-
    open(string(Text),read,Stream,[alias(data)]),
    highlight_and_convert_stream(Stream, LTsf).

    highlight_and_convert_stream(Stream, LTsf) :-
    scan_stream(Stream,Ts),
    close(Stream),
    symbols(Ts,LTsf).

user:portray_message(A,B):-
    lsp(on),
    !,
    writeln(user_error,B),
    (
     q_msg(A,B,T),
     A \= "ignored"
    ->
     assert(m(T))
      ;
    true
    ).




