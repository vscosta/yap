/**
  * @file verify.yap
  *
  * @brief JUpyter support: checking for errors in the text.
  */


 :- module( verify,
              [errors/2,
               ready/2]).

:- use_module(library(hacks)).
%% :- use_module(library(jupyter)).

:-	 use_module(library(lists)).
:-	 use_module(library(maplist)).
:-	 use_module(library(matrix)).
:-	 use_module(library(yapi)).
:-	 use_module(library(python)).

%
%% :-	 use_module(library(python)).
%% :-	 use_module(library(yapi)).

ready( Engine, Query) :-
        errors( Engine , Query ),
        L := Query,
		 L  = [].



errors( Text, _Engine ) :-
    atom_concat('#!python',_, Text),
    !.
errors( Text, _Engine ) :-
    atom_concat('%%',_, Text),
    !.
errors( Text, Engine ) :-
    atom_concat(['%',_,'\n',Extra], Text),
    !,
    errors(Extra,Engine).
errors( Text, Engine ) :-
    open(atom(Text), read, S),
    repeat,
   catch( read_clause(S,T,[syntax_errors(exception)]), E, add(E,Engine)),
    (
	T == end_of_file
    ->
    collect_warnings(Engine),
    close(S),
    !
    ;
    fail
    ).


add(Error, Self) :-
    Error = error(syntax_error(Culprit),Info),
    !,
    yap_error_descriptor(Info,I),
    Dict0 = (end=[]),
    (atom(Culprit), Culprit \= [] -> Label=Culprit;Label='Syntax Error'),
    foldl(add2dict, [label=Label|I], Dict0,  Dict),
    Self.errors := Self.errors+[{Dict}],
    !.
add(E, _Self) :-
    writeln(user_error,E).

add2dict(A=B,Dict,(A:B,Dict)).

user:message_hook(error(style_check(What,File,Line,Clause),Info), _Severity, []) :-
        yap_error_descriptor(Info,I),
	assert(warning([label=What,parserFile=File,parserLine=Line,parserClause=Clause|I])).

collect_warnings(Self) :-
    retract( warning(L) ),
    Dict0 = (end=[]),
    foldl(add2dict, L, Dict0,  Dict),
    Self.warnings := Self.warnings+[{Dict}],
    fail.
 collect_warnings(_Self).


  blank(Text) :-
      atom(Text),
      !,
      atom_codes(Text, L),
      maplist( code_type(space), L).
  blank(Text) :-
      string(Text),
      !,
      string_codes(Text, L),
      maplist( code_type(space), L).
