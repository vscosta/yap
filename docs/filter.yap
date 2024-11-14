
main :-
    unix(argv([File])),
    open(File,read,S),
    findall(O, entry(S,O), Info),
    predcates(Info, _, Preds, _),
    maplist(output,Preds).

entry(S,O) :-
    repeat,
    Vs=[],
     read_clause(S,T,[comments(Comments)]),
     (
     T == end_of_file
     ->
     !,
      fail
     ;
     T = ( :- Directive )
     ->
     O = directive(Directive, Comments, Vs)
     ;
     T = (( Grammar --> Expansion ))
     ->
     functor(Grammar,Name,Arity),
     O = grammar_rule(Name//Arity,Grammar, Expansion, Comments, Vs)
     ;
     T = (( Head :- Body ))
     ->
     functor(Head,Name,Arity),
     O = clause(Name/Arity,Head,Body,Comments,Vs)
     ;
     O=clause(Name/Arity,Head,true,Comments,Vs)
     ).

%% initial directive
predicates([],_,[],[]).
predicates([directive(Directive,Comments,Vs)|More],Ctx,
	   [command(N/A,[comments(Comments),  body(Directive, Vs,Ctx),[]])|Preds], L0) :-
    Ctx \= N/A,
    !,
    predicates(More,N/A,Preds,L0).
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

output(command(_ ,[comments(Comments) |_])) :-
	   maplist(out_comment,Comments,_ ).
output(predicate(N/A,[comments(Comments) |_Clauses])) :-
	   maplist(out_comment,Comments,Found),
	   addcomm(N/A,Found),
	   findall(I,between(1,A,I),Is),
	   maplist(atomic_concat('int ARG'),Is,NIs),
	   T =.. [N|NIs],
	   format('static bool ~w { }~n',[T]).
	   

out_comment(C) :- format('~s',[C]).

addcom(N/A,false) :-
    length(L,A),
    maplist(bind,'?'),
    T =.. [N|L],
    format('~n~n@pred ~q.  (undocumented)~n',[T]).
     
addcom(_N/_A,_Found) :-
	       !.
