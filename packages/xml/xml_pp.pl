/* xml_pp: "pretty print" an XML Document on the current output stream.
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
 */

:- ensure_loaded( xml_utilities ).

/** xml_pp( +XMLDocument )
 *
 * "pretty prints" XMLDocument on the current
 * output stream.
 */
xml_pp( xml(Attributes, Document) ) :-
	write( 'xml( ' ), pp_attributes( Attributes, 0 ), pp_comma, nl,
	pp_list( Document, s(0) ),
	write( ' ).' ), nl.
xml_pp( malformed(Attributes, Document) ) :-
	write( 'malformed( ' ), pp_attributes( Attributes, 0 ), pp_comma, nl,
	pp_list( Document, s(0) ),
	write( ' ).' ), nl.

pp_indented( [], Indent ) :-
	pp_indent( Indent), write( '[]' ).
pp_indented( List, Indent ) :-
	List = [_|_],
	pp_indent( Indent ),
	pp_list( List, Indent ).
pp_indented( comment(Text), Indent ) :-
	pp_indent( Indent ), write( 'comment(' ), pp_string(Text), write( ')' ).
pp_indented( namespace(URI,Prefix,Element), Indent ) :-
	pp_indent( Indent ),
	write( 'namespace( ' ), writeq( URI ), pp_comma_sp,
	pp_string( Prefix ), pp_comma, nl,
	pp_indented( Element, s(Indent) ), nl,
	pp_indent( s(Indent) ), write( ')' ).
pp_indented( element(Tag,Attributes,Contents), Indent ) :-
	pp_indent( Indent ), write( 'element( ' ), writeq( Tag ), pp_comma, nl,
	pp_attributes( Attributes, s(Indent) ), pp_comma, nl,
	pp_list( Contents, s(Indent) ), write( ' )' ).
pp_indented( instructions(Target, Processing), Indent ) :-
	pp_indent( Indent ), write( 'instructions( ' ), writeq( Target ), pp_comma_sp,
	pp_string(Processing), write( ')' ).
pp_indented( doctype(Name, DoctypeId), Indent ) :-
	pp_indent( Indent ), write( 'doctype( ' ), writeq( Name ), pp_comma_sp,
	pp_indented( DoctypeId, s(Indent) ), %'
	write( ' )' ).
pp_indented( cdata(CData), Indent ) :-
	pp_indent( Indent ), write( 'cdata(' ), pp_string(CData), write( ')' ).
pp_indented( pcdata(PCData), Indent ) :-
	pp_indent( Indent ), write( 'pcdata(' ), pp_string(PCData), write( ')' ).
pp_indented( public(URN,URL), _Indent ) :-
	write( 'public(' ), pp_string(URN), pp_comma_sp,
	pp_string(URL), write( ')' ).
pp_indented( public(URN,URL,Literals), Indent ) :-
	write( 'public(' ), pp_string(URN), pp_comma_sp,
	pp_string(URL), pp_list( Literals, s(Indent) ), write( ')' ).
pp_indented( system(URL), _Indent ) :-
	write( 'system(' ), pp_string(URL), write( ')' ).
pp_indented( system(URL,Literals), Indent ) :-
	write( 'system(' ), pp_string(URL), pp_comma_sp,
	pp_list( Literals, s(Indent) ), write( ')' ).
pp_indented( local, _Indent ) :-
	write( local ).
pp_indented( local(Literals), Indent ) :-
	write( 'local(' ), nl,
	pp_list( Literals, s(Indent) ), write( ')' ).
pp_indented( dtd_literal(String), Indent ) :-
	pp_indent( Indent ), write( 'dtd_literal(' ), pp_string(String), write( ')' ).
pp_indented( out_of_context(Tag), Indent ) :-
	pp_indent( Indent ), write( '/* SYNTAX ERROR */ out_of_context( ' ),
	writeq( Tag ), write( ' )' ).
pp_indented( unparsed(String), Indent ) :-
	pp_indent( Indent ), write( '/* SYNTAX ERROR */ unparsed( ' ),
	pp_string(String), write( ' )' ).

pp_list( [], Indent ) :-
	pp_indent( Indent ), write( [] ).
pp_list( [H|T], Indent ) :-
	pp_indent( Indent ), write( '[' ), nl,
	pp_indented( H, Indent ),
	pp_list1( T, Indent ),
	pp_indent( Indent ), write( ']' ).

pp_list1( [], _Indent ) :-
	nl.
pp_list1( [H|T], Indent ) :-
	pp_comma, nl,
	pp_indented( H, Indent ),
	pp_list1( T, Indent ).

pp_attributes( [], Indent ) :-
	pp_indent( Indent ), write( [] ).
pp_attributes( [Attribute|Attributes], Indent ) :-
	pp_indent( Indent ), write( '[' ),
	pp_attributes1( Attributes, Attribute ),
	write( ']' ).

pp_attributes1( [], Name=Value ) :-
	pp_name( Name ), pp_string( Value ).
pp_attributes1( [H|T], Name=Value ) :-
	pp_name( Name ), pp_string( Value ), pp_comma_sp,
	pp_attributes1( T, H ).


pp_name( Name ) :-
	( possible_operator( Name ) ->
		write( '(' ), write( Name ), write( ')=' )
	; otherwise ->
		writeq( Name ), write( '=' )
	).

possible_operator( (abolish) ).
possible_operator( (attribute) ).
possible_operator( (check_advice) ).
possible_operator( (compile_command) ).
possible_operator( (delay) ).
possible_operator( (demon) ).
possible_operator( (discontiguous) ).
possible_operator( (div) ).
possible_operator( (do) ).
possible_operator( (document_export) ).
possible_operator( (document_import) ).
possible_operator( (dy) ).
possible_operator( (dynamic) ).
possible_operator( (edb) ).
possible_operator( (eexport) ).
possible_operator( (else) ).
possible_operator( (except) ).
possible_operator( (export) ).
possible_operator( (foreign_pred) ).
possible_operator( (from) ).
possible_operator( (from_chars) ).
possible_operator( (from_file) ).
possible_operator( (from_stream) ).
possible_operator( (global) ).
possible_operator( (help) ).
possible_operator( (hilog) ).
possible_operator( (if) ).
possible_operator( (import) ).
possible_operator( (index) ).
possible_operator( (initialization) ).
possible_operator( (is) ).
possible_operator( (listing) ).
possible_operator( (local) ).
possible_operator( (locked) ).
possible_operator( (meta_predicate) ).
possible_operator( (mod) ).
possible_operator( (mode) ).
possible_operator( (module_transparent) ).
possible_operator( (multifile) ).
possible_operator( (namic) ).
possible_operator( (nocheck_advice) ).
possible_operator( (nospy) ).
possible_operator( (not) ).
possible_operator( (of) ).
possible_operator( (once) ).
possible_operator( (onto_chars) ).
possible_operator( (onto_file) ).
possible_operator( (onto_stream) ).
possible_operator( (parallel) ).
possible_operator( (public) ).
possible_operator( (r) ).
possible_operator( (rem) ).
possible_operator( (skipped) ).
possible_operator( (spy) ).
possible_operator( (table) ).
possible_operator( (then) ).
possible_operator( (thread_local) ).
possible_operator( (ti) ).
possible_operator( (ti_off) ).
possible_operator( (traceable) ).
possible_operator( (unskipped) ).
possible_operator( (untraceable) ).
possible_operator( (use_subsumptive_tabling) ).
possible_operator( (use_variant_tabling) ).
possible_operator( (volatile) ).
possible_operator( (with) ).
possible_operator( (with_input_from_chars) ).
possible_operator( (with_output_to_chars) ).
possible_operator( (xor) ).

pp_indent( 0 ).
pp_indent( s(N) ) :-
	write( '	' ),
	pp_indent( N ).

pp_comma :-
	write( ',' ).

pp_comma_sp :-
	write( ', ' ).

