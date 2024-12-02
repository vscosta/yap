#!/home/vsc/.local/bin/yap -L --

/** @file filter.yap
 *
 * This filter extends doxygen with YAP documentation support.
 *
 */


:- dynamic exported/2, defines_module/1.

:- use_module(library(lists)).
:- use_module(library(maplist)).
:- use_module(library(system)).
:- use_module(library(matrix)).
:- use_module(library(readutil)).

valid_suffix('.yap').
valid_suffix('.pl').
valid_suffix('.prolog').
valid_suffix('.P').

/**
 * @pred main
 *
 * Call the filter.
*/

main :-
    unix(argv([File])),
    absolute_file_name(File, Y, [access(read),file_type(prolog),file_errors(fail),solutions(first)]),
    valid_suffix(ValidSuffix),
    sub_atom(Y,_,_,0,ValidSuffix),
    !,
    file_directory_name(File, Dir),
    working_directory(OldD,Dir),
    open(Y,read,S),
    script(S),
    findall(O, entry(S,O), Info),
    predicates(Info, _, Preds, _),
    insert_module_header,
    maplist(output,Preds),
    working_directory(_,OldD),
    insert_module_tail,
    halt.
main :-
    halt.
/*    unix(argv([File])),
    read_file_to_string(File,Text),
    format('~s',[Text]),
halt.
*/

/*
atom_concat('cat ',File,Command),
unix(system(Command)).


    read_stream_to_string(S,Text),
    format('~s',[Text]).
*/

%%
%% @pred script(+S)
%%
script(S) :-
    peek_char(S,'#'),
    !,
    read_line_to_string(S,_),
    script(S).
script(_).

/**
 * @pred entry(+Stream, -Units).
 * Obtain text units
 */
entry(S,O) :-
    repeat,
    read_clause(S,T,[comments(Comments),variable_names(Vs)]),
    (
      T == end_of_file
      ->
      !
      ;
      T = ( :- Directive )
      ->
      dxpand(Directive),
      O = directive(Directive, Comments, Vs)
      ;

      T = (( Grammar --> Expansion ))
      ->
      functor(Grammar,Name,Arity),
      A is Arity+2,
      O = clause(Name/A, Grammar, Expansion, Comments, Vs)
      ;
      T = (( Head :- Body ))
      ->
      functor(Head,Name,Arity),
      O = clause(Name/Arity,Head,Body,Comments,Vs)
      ;
      O=clause(Name/Arity,Head,true,Comments,Vs)
    ).

%% initial directive
% predicates([H|_],_,_,_) :-
%     writeln(H),
%     fail.
predicates([],_,[],[]).
predicates([directive(Directive,Comments,Vs)|More],Ctx,
	   [command([comments(Comments),  body(Directive, Vs,Ctx),[]])|Preds], L0) :-
    !,
    predicates(More,0,Preds,L0).
% change of predicate
predicates([clause(N/A,Head,Body,Comments,Vs)|More],Ctx,
	   [predicate(N/A,[comments(Comments), head_body(Head, Body, Vs)|L0])|Preds], L0) :-
    Ctx \= N/A,
    !,
    predicates(More,N/A,Preds,L0).
% new predicate
predicates([clause(N/A,Head,Body,Comments,Vs)|More],[],

	   [predicate(N/A,[comments(Comments), head_body(Head, Body, Vs)|L0])|Preds], L0) :-
    !,
    predicates(More,N/A,Preds,L0).
% predicate continuation
predicates([clause(N/A,Head,Body,Comments,Vs)|More],N/A,
	   Preds, [comments(Comments), head_body(Head, Body, Vs)|L0]) :-
    predicates(More,N/A,Preds,L0).

output(command([comments(Comments) |_])) :-
    maplist(out_comment(_),Comments ).
output(predicate(N/A,[comments(Comments) |_Clauses])) :-
%    atom_chars(N,Ns),
%    foldl(csafe,Ns,SNs,[]),
    %    format(atom(N1),'~s/~d',[SNs,A]),
    N1=N,
    maplist(out_comment(Found),Comments),
    addcomm(N1/A,N/A,Found),
    findall(I,between(1,A,I),Is),
    maplist(atomic_concat('int ARG'),Is,NIs),
    (
      A==0 ->
      T = N()
      ;
      T =.. [N1|NIs]
    ),
    
    format(' class  YAP~s { ~n YAP~w;~n};~n~n~n',[ N,T]).

insert_module_header :-
%    format('class Predicate;~n~n',[]),
    defines_module(M),
    !,
    format('namespace ~s~n{~n',[M]).
insert_module_header.

insert_module_tail :-
    defines_module(_M),
    !,
    format('}~n',[]).
insert_module_tail.



out_comment(true,C) :-
    string_chars(C,Cs),
    trl(Cs,Cf),
    !,
    format('~s~n',[Cf]).
out_comment(_,C) :-
    format('~s~n',[C  ]).

trl( ['/','*','<',C|L],['/','*','<',C|NL]) :-
    sp(C),
    !,
    trl_in(L,NL).
trl( ['/','*','*',C|L],['/','*','*',C|NL]) :-
    sp(C),
    !,
    trl_in(L,NL).
trl( ['%','%','<',C|L],['/','/','/','<',C|NL]) :-
    sp(C),
    !,
    trl_in(L,NL).
trl( ['%','%',C|L],['/','/','/',C|NL]) :-
    sp(C),
    !,
    trl_in(L,NL).
trl( L,L).

sp(' ').
sp('\t').
sp('\n').

trl_in(RL,NL) :-
    (
      append(Start,['@',p,r,e,d|Decl0],RL)
      ->
      scan_line(Decl0,Line,Rest),
      skip_blanks(Line,Decl),
      decl(Decl,DL),
      append([Start,DL,['\n'|Rest]],NL)
      ;
      NL=RL
    ).

scan_line([],[],[]).

scan_line(['\n'|R],[],R) :-
    !.
scan_line([C|Cs],[C|M],R) :-
    scan_line(Cs,M, R).

skip_blanks([C|Cs], R) :-
    sp(C),
    !,
    skip_blanks(Cs,R).
skip_blanks(Cs,Cs).


decl(D,F) :-
    append(Name,['('|D1],D),
    Name = [_|_],
    !,
    append(Info,[')'|R1],D1),
    foldl(arity,Info,1,Arity),
%    foldl(csafe,Name,TName,[]),
    format(chars(F),'@class YAP~s [~s/~d](@ref YAP~s)~n @brief  <b>~s(~s)</b> ~s~n ',  [Name,Name,Arity,Name,Name,Info,R1]).
decl(D,F) :-
    append(Name,[C|D1],D),
    sp(C),
    Name = [_|_],
    !,
    %foldl(csafe,Name,TName,[]),
    format(chars(F,D1),'@class YAP~s [~s/0](@ref YAP~s)~n @brief <b>~s()</b>~n ~s~n',[Name,Name,Name,Name,[C|D1]]).

    /* This routine generates two streams from the comment:
 * - the class text.
 * - The source
 */
arity(',',I, I1) :-
    !,
    I1 is I+1.
arity(_,I,I).

csafe(C,LF,L0) :-
    char_to_safe(C,LF,L0),
    !.
csafe(C,[C|L],L).

char_to_safe('=',['_',e,q|L],L).
char_to_safe('<',['_',l,t|L],L).
char_to_safe('>',['_',g,t|L],L).
%char_to_safe('_',['_','_'|L],L).
char_to_safe('!',['_',c,t|L],L).
char_to_safe('-',['_',m,n|L],L).
char_to_safe('+',['_',p,l|L],L).
char_to_safe('*',['_',s,t|L],L).
char_to_safe('/',['_',s,l|L],L).
char_to_safe('$',['_',d,l|L],L).

addcomm(_N/A,N0/A,false) :-
    is_exported(N0,A),
    !,
    length(L,A),
    maplist(=('?'),L),
    T =.. [N0|L],
    format('~n~n/**   @class YAP~s [~s/~d](@ref YAP~s)~n @brief <b>~w</b>  (undocumented)  **/~n~n~n~n',[N0,N0,A,N0,T]).
addcomm(_,_,_).

:- initialization(main).

dxpand(module(M,Gs)) :-
    module(M),
    assert(defines_module(M)),
    maplist(dxpand,Gs).
dxpand(op(M,Gs,Y)) :-
    op(M,Gs,Y).
dxpand(use_module(M,Gs)) :-
    (:-use_module(M,Gs)).
dxpand(module(user)) :- !.
dxpand(module(M)) :-
    ensure_loaded(M).
dxpand(A/B) :-assert(exported(A,B)).
dxpand(A//B):- B2 is B+2, assert(exported(A,B2)) .

is_exported(N,_) :-
    string(N),
    string_concat(`$`,_,N),
    !,
    fail.
is_exported(N,_) :-
    atom(N),
    atom_concat('$',_,N),
    !,
    fail.
is_exported(_,_) :-
    current_source_module(_,user),
    !.
is_exported(N,A) :-
    exported(N,A).
