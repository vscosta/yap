/** 
 @file xml2yap.yap
 @author V Santos Costa

 @defgroup XML4PL XML to Prolog parser
 @ingroup YAPPackages

 @brief Load XML files using the YAP C++ interface and the PUGI library.

This library imports a XML file as a Prolog term.

*/
:- module( xml4yap, [load_xml/2,
		     xml_load/2,
		     xml_pretty_print/1,
		     op(50, yf, []),
		     op(50, yf, '()'),
     op(50, yf, '{}'),
     op(100, xfy, '.'),
     op(100, fy, '.')
] ).


:- use_module(library(maplist)).


:- load_foreign_files([],['YAPxml'],libxml_yap_init).

xml_pretty_print(S) :-
    load_xml(S,Gs),
    maplist(pp(0),Gs).

pp(D,Node) :-
    Node=..[Name,[L|Tree]],
    format(',~n~*c~q( ~q ',[D,0' ,Name,L]),
    D1 is D+1,
    gopp(D1,Tree),
    format(' )',[]).


gopp(_D,[Tree]) :-
    !,
    format(', ~q',[Tree]).
gopp(D1,Tree) :-
    maplist(pp(D1),Tree),
    !.
gopp(_D,Tree) :- format(', ~q',[Tree]).

	
