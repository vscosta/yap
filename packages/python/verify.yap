/**
  * @file verify.yap
  *
  * @brief JUpyter support: checking for errors in the text.
  */


 :- module( verify,
              [errors/2,
               ready/2]
                       ).
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

errors( Text, Engine ) :-
    start_low_level_trace,
    open(atom(Text), read, S),
    repeat,
    catch(read_term(S,T,[syntax_errors(exception)]),E,add(E, Engine)),
    stoop_low_level_trace,
    (
	T == end_of_file
    ->
    close(S),
    !
    ;
    fail
    ).


add(error(syntax_error(Culprit),Info), Self) :-
    yap_error_descriptor(Info,I),
    Dict0 = (end=[]),
    (atom(Culprit), Culprit \= [] -> Label=Culprit;Label='Syntax Error'),
<<<<<<< HEAD
    writeln(I),
    D :=  dict([label=Label|I]),
    writeln((D :=  dict([label=Label|I]))),
    := Engine.errors.append(D).
=======
    foldl(add2dict, [label=Label|I], Dict0,  Dict),
    Self.errors := Self.errors+[{Dict}].

add2dict(A=B,Dict,(A:B,Dict)).
>>>>>>> 495eba1bd79ee73f40cb4862d2d00ad1b67a6a26


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
