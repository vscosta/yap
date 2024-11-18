#!/Users/vsc/.local/bin/yap -L --
# zero.
#

/** @file filter.yap
 *
 * This filter extends doxygen with YAP documentation support.
 *
 */

:- use_module(library(maplist)).

/**
 * @pred main
 *
 * Call the filter.
*/
main :-
    unix(argv([File])),
    absolute_file_name(File, Y, [access(read),file_type(prolog),file_errors(fail),solutions(first)]),
    !,
    open(Y,read,S),
    findall(O, entry(S,O), Info),
    predicates(Info, _, Preds, _),
    maplist(output,Preds).
main :-
    open(File,read,S),
    read_stream_to_string(S,Text),
    format('~s',[Text]).
    
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
predicates([H|_],_,_,_) :-
    writeln(H),
    fail.
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

addcomm(N/A,false) :-
    !,
    length(L,A),
    maplist(=('?'),L),
    T =.. [N|L],
    format('~n~n/** @pred ~q.  (undocumented)  **/~n',[T]).
addcomm(_,_).

:- initialization(main).
