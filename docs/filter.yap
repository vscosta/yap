#!/home/vsc/.local/bin/yap -L --

/** @file filter.yap
 *
 * This filter extends doxygen with YAP documentation support.
 *
 */


:- dynamic exported/2.

:- use_module(library(maplist)).
:- use_module(library(system)).
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
    maplist(output,Preds),
    working_directory(_,OldD),
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
script(S) :-
peek_char(S,#),
readutil:read_line_to_string(S,_),
script(S).
script(_).

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
    dxpand(Directive), O = directive(Directive, Comments, Vs)
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
	   maplist(out_comment(Found),Comments),
	   addcomm(N/A,Found),
	   findall(I,between(1,A,I),Is),
	   maplist(atomic_concat('int ARG'),Is,NIs),
	   T =.. [N|NIs],
	   format('class  ~w {~n ~w ~w;~n};~n',[N,N,T]).
	   

out_comment(_,C) :-
    format('~s~n',[C]),
    fail.
out_comment(true,C) :-
    sub_string(C,_,_,_,`@pred`),
    !.
out_comment(_,_C).

addcomm(N/0,false) :-
exported(N,0),
!,
    format('~n~n/** @pred Predicate~q().  (undocumented)  **/~n',[N]).
addcomm(N/A,false) :-
exported(N,A),
    !,
    length(L,A),
    maplist(=('?'),L),
    T =.. [N|L],
    format('~n~n/** @pred Predicate~q.  (undocumented)  **/~n',[T]).
addcomm(_,_).

:- initialization(main).

dxpand(module(M,Gs)) :-
    module(M),
maplist(dxpand,Gs).
dxpand(op(M,Gs,Y)) :-
    op(M,Gs,Y).
dxpand(use_module(M,Gs)) :-
    (:-use_module(M,Gs)).
dxpand(use_module(M)) :-
use_module(M).
dxpand(A/B) :-assert(exported(A,B)).
dxpand(A//B):- B2 is B+2, assert(exported(A,B2)) .

