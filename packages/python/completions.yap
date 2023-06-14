 /**
 *  @file completer.yap
 *
 *  @brief Prolog completer interface to Python.
 */


:- module( completions,
	   [
	       completions/3,
	       completions/4
	   ]).

:-	 use_module(library(python)).
:-	 use_module(library(completer)).
:-	 use_module(library(maplist)).

/**
 *  @defgroup YAPCompleter  Call Prolog completer from Python
 *  @ingroup YAPPython
 *  @{
 *
 *  @brief Prolog completer from Python.
 *
 *  Help the user by trying to guess what she will type next.
 */

%% @pred completions( +Text, +PythonCompleterObject )
% 
% Tries to complete the current text. The list with the set of completions
% is stored in the
% `matches` field of the python object.
%
completions(_S, Line, Pos, Self) :-
    sub_string(Line,0,Pos,_,Text),
    completer(Text,Cs),
    maplist(atom_concat(Text), Cs, FCs),
    (
    var(Self)-> Self = FCs ; Self.matches := FCs
    ).


completions(Line, Pos, Self) :-
	completions(_S, Line, Pos, Self).	
%% @}
