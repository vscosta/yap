/* xml.pl : XML Module wrapper for Quintus Prolog.
 *
 * Copyright (C) 2001-2005 Binding Time Limited
 * Copyright (C) 2005-2011 John Fletcher
 *
 * Current Release: $Revision: 3.3 $
 * 
 * TERMS AND CONDITIONS:
 *
 * This program is offered free of charge, as unsupported source code. You may
 * use it, copy it, distribute it, modify it or sell it without restriction,
 * but entirely at your own risk.
 *
 */
:- module( xml,
	[
	xml_parse/2,
	xml_parse/3,
	xml_subterm/2,
     xml_pp/1,
     load_xml/3,
     load_xml/2
	] ).

/* @section xml Prolog XML module
 * xml is intended to be a rather modular module: it should be easy to
 * build a program that can output XML, but not read it, or vice versa.
 * Similarly, you may be happy to dispense with diagnosis once you are
 * sure that your code will only try to make valid calls to xml_parse/2.
 *
 * It is intended that the code should be very portable too. Clearly,
 * some small changes will be needed between platforms, but these should
 * be limited to xml_utilities. xml_utilities contains most of the shared
 * code and most of the potentially non-portable code.
 */

:- use_module( library(lists), [append/3] ).
:- use_module( library(readutil) ).


:- ensure_loaded( xml/xml_driver ).



/* atom_codes/2, number_codes/2 and throw/1 are ISO predicates, mapped to
 * the Quintus equivalent here.
 */
%atom_codes( Atom, Codes ) :-
%	atom_chars( Atom, Codes ).

%number_codes( Number, Codes ) :-
%	number_chars( Number, Codes ).

/** @pred xml_exception( +Message, +Document, +Culprit, +Path )
 *  a hook to
 * raise an exception to be raised in respect of a fault in the XML Term:
 * Document.
 *  - Culprit is a sub-term of Document which cannot be serialized;
 *  - Message is an atom naming the type of error;
 *  - Path is a string encoding a list of SubTerm's ancestor elements in the
 *    form <tag>{(id)}* where <tag> is the element tag and <id> is the value
 *    of any attribute _named_ id.
 */
xml_exception( Message, Document, Culprit, Path ) :-
	raise_exception(
		application_error('XML Parse: ~s in ~q~nCulprit: ~q~nPath: ~s', 
			[Message,Document,Culprit,Path] )
		).

load_xml(File, XML, []) :-
    open( File, read, S),
    read_stream_to_codes(S, Doc),
    close(S),
    xml_parse(Doc, XML).

load_xml(File, XML) :-
    open( File, read, S),
    read_stream_to_codes(S, Doc),
    close(S),
    xml_parse(Doc, XML).


/* member( ?Element, ?List ) holds when Element is a member of List.
 */
member( H, [H|_] ).
member( H, [_|T] ):-
    member( H, T ).

/* select( ?Element, ?List0, ?List1 ) is true if List1 is equal to List0
 * with Element removed.
 */
select( H, [H|T], T ).
select( Element, [H|T0], [H|T1] ):-
    select( Element, T0, T1 ).

/* is_list( +List ) holds when List is a list.
 */
%is_list( List ) :-
%	nonvar( List ),
%	is_list1( List ).

%is_list1( [] ).
%is_list1( [_|_] ).

