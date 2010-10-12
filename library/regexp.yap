/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		regexp.yap						 *
* Last rev:	3/22/2000						 *
* mods:									 *
* comments:	Support for Regular Expressions	in YAP			 *
*									 *
*************************************************************************/

:- module(regexp, [
	regexp/3,
	regexp/4
          ]).

:- load_foreign_files([regexp], [], init_regexp).

regexp(RegExp, String, Opts) :-
	length(RegExp, LRE),
	length(String, LS),
	check_opts(Opts,0,IOpts,regexp(RegExp, String, Opts)),
	check_regexp(RegExp,LRE,String,LS,IOpts).

regexp(RegExp, String, Opts, OUT) :-
	length(RegExp, LRE),
	length(String, LS),
	check_out(OUT,0,Count,regexp(RegExp, String, Opts, OUT)),
	check_opts(Opts,0,IOpts,regexp(RegExp, String, Opts, OUT)),
	check_regexp(RegExp,LRE,String,LS,IOpts,OUT,Count).

%
% OUT must be bound to a list of unbound variables.
% Check this and count how many.
%
check_out(V,_,_,_) :- var(V), !.
check_out([],I,I,_) :- !.
check_out([V|L],I0,IF,G) :- !,
	(nonvar(V) -> throw(error(uninstantiation_error(V),G)) ; true),
	I is I0+1,
	check_out(L,I,IF,G).
check_out(OUT,_,_,G) :-
	throw(error(uninstantiation_error(OUT),G)).

%
% Option processing
%
check_opts(V,_,_,G) :- var(V), !,
	throw(error(instantiation_error,G)).
check_opts([],I,I,_) :- !.
check_opts([A|L],I0,IF,G) :- !,
	process_opt(A,I1,G),
	I is I0+I1,
	check_opts(L,I,IF,G).
check_opts(Opts,_,_,G) :-
	throw(error(type_error(variable,Opts),G)).

process_opt(V,_,G) :- var(V), !,
	throw(error(instantiation_error,G)).
process_opt(nocase,1,_) :- !.
process_opt(indices,2,_) :- !.
process_opt(I,_,G) :-
	throw(error(domain_error(flag_value,regexp_options+I),G)).


