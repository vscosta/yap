 /**
 *  @file completer.yap
 *
 *  @brief Prolog completer interface to Python.
 */


:- module( completions,
	   [
	       completions/2
	   ]).
:-	 use_module(library(completer)).
:-	 use_module(library(maplist)).

/**
 *  @defgroup YAPCompleter  Call Prolog completer from Python
 *  @ingroup YAP4Py
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
completions(Text, FCs) :-
    completer(Text,Cs),
    maplist(atom_string, Cs, FCs).

	
%% @}
