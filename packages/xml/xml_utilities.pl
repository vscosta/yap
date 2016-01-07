/* XML Utilities
 *
 * Copyright (C) 2001-2005 Binding Time Limited
 * Copyright (C) 2005-2011 John Fletcher
 *
 * Current Release: $Revision: 3.4 $
 * 
 * TERMS AND CONDITIONS:
 *
 * This program is offered free of charge, as unsupported source code. You may
 * use it, copy it, distribute it, modify it or sell it without restriction,
 * but entirely at your own risk.
 */


:- use_module(library(lists)).

% Entity and Namespace map operations: these maps are usually quite small, so
% a linear list lookup is okay. They could be substituted by a logarithmic
% data structure - in extremis.

/** empty_map( ?Map )
 *
 * is true if Map is a null map.
 */
empty_map( [] ).

/* map_member( +Key, +Map, ?Data )
 *
 * is true if Map is a ordered map structure
 * which records the pair Key-Data. Key must be ground.
 */
map_member( Key0, [Key1-Data1|Rest], Data0 ) :-
	( Key0 == Key1 ->
		Data0 = Data1
	; Key0 @> Key1 ->
		map_member( Key0, Rest, Data0 )
	).

/* map_store( +Map0, +Key, +Data, ?Map1 )
 *
 *
 * is true if Map0 is an ordered map
 * structure, Key must be ground, and Map1 is identical to Map0 except that
 * the pair Key-Data is recorded by Map1.
 */
map_store( [], Key, Data, [Key-Data] ).
map_store( [Key0-Data0|Map0], Key, Data, Map ) :-
	( Key == Key0 ->
		Map = [Key-Data|Map0]
	; Key @< Key0 ->
		Map = [Key-Data,Key0-Data0|Map0]
	; otherwise -> % >
		Map = [Key0-Data0|Map1],
		map_store( Map0, Key, Data, Map1 )
	).

/** context(?Element, ?PreserveSpace, ?CurrentNS, ?DefaultNS, ?Entities, ?Namespaces )
 *
 * is an ADT hiding the "state" arguments for XML Acquisition
 */
initial_context(
		Controls,
		context(void,PreserveSpace,'','',Entities,Empty,
			RemoveAttributePrefixes,AllowAmpersand)
		) :-
	empty_map( Empty ),
	( member( extended_characters(false), Controls ) ->
		Entities = Empty
	; otherwise ->
		extended_character_entities(Entities)
	),
	( member( format(false), Controls ) ->
		PreserveSpace = true
	; otherwise ->
		PreserveSpace = false
	),
	( member( remove_attribute_prefixes(true), Controls ) ->
		RemoveAttributePrefixes = true
	; otherwise ->
		RemoveAttributePrefixes = false
	),
	( member( allow_ampersand(true), Controls ) ->
		AllowAmpersand = true
	; otherwise ->
		AllowAmpersand = false
	).

context_update( current_namespace, Context0, URI, Context1 ) :-
	Context0 = context(Element,Preserve,_Current,Default,Entities,
		Namespaces,RemoveAttributePrefixes,Amp),
	Context1 = context(Element,Preserve,URI,Default,Entities,
		Namespaces,RemoveAttributePrefixes,Amp).
context_update( element, Context0, Tag, Context1 ) :-
	Context0 = context(_Element,Preserve,Current,Default,Entities,
		Namespaces,RemoveAttributePrefixes,Amp),
	Context1 = context(tag(Tag),Preserve,Current,Default,Entities,
		Namespaces,RemoveAttributePrefixes,Amp).
context_update( default_namespace, Context0, URI, Context1 ):-
	Context0 = context(Element,Preserve,Current,_Default,Entities,
		Namespaces,RemoveAttributePrefixes,Amp),
	Context1 = context(Element,Preserve,Current,URI,Entities,
		Namespaces,RemoveAttributePrefixes,Amp).
context_update( space_preserve, Context0, Boolean, Context1 ):-
	Context0 = context(Element,_Preserve,Current,Default,Entities,
		Namespaces,RemoveAttributePrefixes,Amp),
	Context1 = context(Element,Boolean,Current,Default,Entities,
		Namespaces,RemoveAttributePrefixes,Amp).
context_update( ns_prefix(Prefix), Context0, URI, Context1 ) :-
	Context0 = context(Element,Preserve,Current,Default,Entities,
		Namespaces0,RemoveAttributePrefixes,Amp),
	Context1 = context(Element,Preserve,Current,Default,Entities,
		Namespaces1,RemoveAttributePrefixes,Amp),
	map_store( Namespaces0, Prefix, URI, Namespaces1 ).
context_update( entity(Name), Context0, String, Context1 ) :-
	Context0 = context(Element,Preserve,Current,Default,Entities0,
		Namespaces,RemoveAttributePrefixes,Amp),
	Context1 = context(Element,Preserve,Current,Default,Entities1,
		Namespaces,RemoveAttributePrefixes,Amp),
	map_store( Entities0, Name, String, Entities1 ).

remove_attribute_prefixes( Context ) :-
	Context = context(_Element,_Preserve,_Current,_Default,_Entities,
		_Namespaces,true,_Amp).

current_tag( Context, Tag ) :-
	Context = context(tag(Tag),_Preserve,_Current,_Default,_Entities,
		_Namespaces,_RPFA,_Amp).

current_namespace( Context, Current ) :-
	Context = context(_Element,_Preserve,Current,_Default,_Entities,
		_Namespaces,_RPFA,_Amp).

default_namespace( Context, Default ) :-
	Context = context(_Element,_Preserve,_Current,Default,_Entities,
		_Namespaces,_RPFA,_Amp).

space_preserve( Context ) :-
	Context = context(tag(_Tag),true,_Current,_Default,_Entities,
		_Namespaces,_RPFA,_Amp).

specific_namespace( Prefix, Context, URI ) :-
	Context = context(_Element,_Preserve,_Current,_Default,_Entities,
		Namespaces,_RPFA,_Amp),
	map_member( Prefix, Namespaces, URI ).

defined_entity( Reference, Context, String ) :-
	Context = context(_Element,_Preserve,_Current,_Default,Entities,
		_Namespaces,_RPFA,_Amp),
	map_member( Reference, Entities, String ).
	
close_context( Context, Terms, WellFormed ) :-
	Context = context(Element,_Preserve,_Current,_Default,_Entities,
		_Namespaces,_RPFA,_Amp),
	close_context1( Element, Terms, WellFormed ).

close_context1( void, [], true ).
close_context1( tag(TagChars), [out_of_context(Tag)], false ) :-
	atom_chars( Tag, TagChars ).

void_context(
	context(void,_Preserve,_Current,_Default,_Entities,_Names,_RPFA,_Amp)
	).

allow_ampersand(
	context(_Void,_Preserve,_Current,_Default,_Entities,_Names,_RPFA,true)
	).

/** pp_string( +String )
 *
 * prints String onto the current output stream.
 * If String contains only 7-bit chars it is printed in shorthand quoted
 * format, otherwise it is written as a list.
 * If your Prolog uses " to delimit a special string type, just use write/1.
 */
pp_string( Chars ) :-
	( member( Char, Chars ),
	  not_shorthand( Char ) ->
		write( Chars )
	; otherwise ->
		put_quote,
		pp_string1( Chars ),
		put_quote
	).

not_shorthand( Char ) :-
	Char > 255.
not_shorthand( Char ) :-
	Char < 9.
not_shorthand( 126 ).	% ~ gives syntax errors in LPA Prolog

put_quote :-
	put( 0'" ). % '

pp_string1( [] ).
pp_string1( [Char|Chars] ) :-
	( Char =:= """"  -> % Meta-quote
		put( Char ),
		put( Char ),
		pp_string1( Chars )
	; Char =:= 13,	% Handle Windows border-settings
	  Chars = [10|Chars1] ->
		put( 10 ),
		pp_string1( Chars1 )
	; otherwise ->
		put( Char ),
		pp_string1( Chars )
	).

xml_declaration_attributes_valid( [] ).
xml_declaration_attributes_valid( [Name=Value|Attributes] ) :-
	xml_declaration_attribute_valid( Name, Value ),
	xml_declaration_attributes_valid( Attributes ).

xml_declaration_attribute_valid( Name, Value ) :-
	lowercase( Value, Lowercase ),
	canonical_xml_declaration_attribute( Name, Lowercase ).

canonical_xml_declaration_attribute( version, "1.0" ).
canonical_xml_declaration_attribute( standalone, "yes" ).
canonical_xml_declaration_attribute( standalone, "no" ).
% The encodings here are all valid for the output produced.
canonical_xml_declaration_attribute( encoding, "utf-8" ).
% canonical_xml_declaration_attribute( encoding, "utf-16" ).
% This is erroneous for the output of this library
canonical_xml_declaration_attribute( encoding, "us-ascii" ).
canonical_xml_declaration_attribute( encoding, "ascii" ).
canonical_xml_declaration_attribute( encoding, "iso-8859-1" ).
canonical_xml_declaration_attribute( encoding, "iso-8859-2" ).
canonical_xml_declaration_attribute( encoding, "iso-8859-15" ).
canonical_xml_declaration_attribute( encoding, "windows-1252" ).
% In general, it's better not to specify an encoding.

/* lowercase( +MixedCase, ?Lowercase ) holds when Lowercase and MixedCase are
 * lists of character codes, and Lowercase is identical to MixedCase with
 * every uppercase character replaced by its lowercase equivalent.
 */
lowercase( [], [] ).
lowercase( [Char|Chars], [Lower|LowerCase] ) :-
	( Char >= "A", Char =< "Z" ->
		Lower is Char + "a" - "A"
	; otherwise ->
		Lower = Char
	),
	lowercase( Chars, LowerCase ).

extended_character_entities( [
	"AElig"-[198],		% latin capital letter AE
	"Aacute"-[193],		% latin capital letter A with acute,
	"Acirc"-[194],		% latin capital letter A with circumflex,
	"Agrave"-[192],		% latin capital letter A with grave
	"Alpha"-[913],		% greek capital letter alpha, U+0391
	"Aring"-[197],		% latin capital letter A with ring above
	"Atilde"-[195],		% latin capital letter A with tilde,
	"Auml"-[196],		% latin capital letter A with diaeresis,
	"Beta"-[914],		% greek capital letter beta, U+0392
	"Ccedil"-[199],		% latin capital letter C with cedilla,
	"Chi"-[935],		% greek capital letter chi, U+03A7
	"Dagger"-[8225],	% double dagger, U+2021 ISOpub
	"Delta"-[916],		% greek capital letter delta,
	"ETH"-[208],		% latin capital letter ETH, U+00D0 ISOlat1>
	"Eacute"-[201],		% latin capital letter E with acute,
	"Ecirc"-[202],		% latin capital letter E with circumflex,
	"Egrave"-[200],		% latin capital letter E with grave,
	"Epsilon"-[917],	% greek capital letter epsilon, U+0395
	"Eta"-[919],		% greek capital letter eta, U+0397
	"Euml"-[203],		% latin capital letter E with diaeresis,
	"Gamma"-[915],		% greek capital letter gamma,
	"Iacute"-[205],		% latin capital letter I with acute,
	"Icirc"-[206],		% latin capital letter I with circumflex,
	"Igrave"-[204],		% latin capital letter I with grave,
	"Iota"-[921],		% greek capital letter iota, U+0399
	"Iuml"-[207],		% latin capital letter I with diaeresis,
	"Kappa"-[922],		% greek capital letter kappa, U+039A
	"Lambda"-[923],		% greek capital letter lambda,
	"Mu"-[924],			% greek capital letter mu, U+039C
	"Ntilde"-[209],		% latin capital letter N with tilde,
	"Nu"-[925],			% greek capital letter nu, U+039D
	"OElig"-[338],		% latin capital ligature OE,
	"Oacute"-[211],		% latin capital letter O with acute,
	"Ocirc"-[212],		% latin capital letter O with circumflex,
	"Ograve"-[210],		% latin capital letter O with grave,
	"Omega"-[937],		% greek capital letter omega,
	"Omicron"-[927],	% greek capital letter omicron, U+039F
	"Oslash"-[216],		% latin capital letter O with stroke
	"Otilde"-[213],		% latin capital letter O with tilde,
	"Ouml"-[214],		% latin capital letter O with diaeresis,
	"Phi"-[934],		% greek capital letter phi,
	"Pi"-[928],			% greek capital letter pi, U+03A0 ISOgrk3
	"Prime"-[8243],		% double prime = seconds = inches,
	"Psi"-[936],		% greek capital letter psi,
	"Rho"-[929],		% greek capital letter rho, U+03A1
	"Scaron"-[352],		% latin capital letter S with caron,
	"Sigma"-[931],		% greek capital letter sigma,
	"THORN"-[222],		% latin capital letter THORN,
	"Tau"-[932],		% greek capital letter tau, U+03A4
	"Theta"-[920],		% greek capital letter theta,
	"Uacute"-[218],		% latin capital letter U with acute,
	"Ucirc"-[219],		% latin capital letter U with circumflex,
	"Ugrave"-[217],		% latin capital letter U with grave,
	"Upsilon"-[933],	% greek capital letter upsilon,
	"Uuml"-[220],		% latin capital letter U with diaeresis,
	"Xi"-[926],			% greek capital letter xi, U+039E ISOgrk3
	"Yacute"-[221],		% latin capital letter Y with acute,
	"Yuml"-[376],		% latin capital letter Y with diaeresis,
	"Zeta"-[918],		% greek capital letter zeta, U+0396
	"aacute"-[225],		% latin small letter a with acute,
	"acirc"-[226],		% latin small letter a with circumflex,
	"acute"-[180],		% acute accent = spacing acute,
	"aelig"-[230],		% latin small letter ae
	"agrave"-[224],		% latin small letter a with grave
	"alefsym"-[8501],	% alef symbol = first transfinite cardinal,
	"alpha"-[945],		% greek small letter alpha,
	"and"-[8743],		% logical and = wedge, U+2227 ISOtech
	"ang"-[8736],		% angle, U+2220 ISOamso
	"aring"-[229],		% latin small letter a with ring above
	"asymp"-[8776],		% almost equal to = asymptotic to,
	"atilde"-[227],		% latin small letter a with tilde,
	"auml"-[228],		% latin small letter a with diaeresis,
	"bdquo"-[8222],		% double low-9 quotation mark, U+201E NEW
	"beta"-[946],		% greek small letter beta, U+03B2 ISOgrk3
	"brvbar"-[166],		% broken bar = broken vertical bar,
	"bull"-[8226],		% bullet = black small circle,
	"cap"-[8745],		% intersection = cap, U+2229 ISOtech
	"ccedil"-[231],		% latin small letter c with cedilla,
	"cedil"-[184],		% cedilla = spacing cedilla, U+00B8 ISOdia>
	"cent"-[162],		% cent sign, U+00A2 ISOnum>
	"chi"-[967],		% greek small letter chi, U+03C7 ISOgrk3
	"circ"-[710],		% modifier letter circumflex accent,
	"clubs"-[9827],		% black club suit = shamrock,
	"cong"-[8773],		% approximately equal to, U+2245 ISOtech
	"copy"-[169],		% copyright sign, U+00A9 ISOnum>
	"crarr"-[8629],		% downwards arrow with corner leftwards
	"cup"-[8746],		% union = cup, U+222A ISOtech
	"curren"-[164],		% currency sign, U+00A4 ISOnum>
	"dArr"-[8659],		% downwards double arrow, U+21D3 ISOamsa
	"dagger"-[8224],	% dagger, U+2020 ISOpub
	"darr"-[8595],		% downwards arrow, U+2193 ISOnum
	"deg"-[176],		% degree sign, U+00B0 ISOnum>
	"delta"-[948],		% greek small letter delta,
	"diams"-[9830],		% black diamond suit, U+2666 ISOpub
	"divide"-[247],		% division sign, U+00F7 ISOnum>
	"eacute"-[233],		% latin small letter e with acute,
	"ecirc"-[234],		% latin small letter e with circumflex,
	"egrave"-[232],		% latin small letter e with grave,
	"empty"-[8709],		% empty set = null set = diameter,
	"emsp"-[8195],		% em space, U+2003 ISOpub
	"ensp"-[8194],		% en space, U+2002 ISOpub
	"epsilon"-[949],	% greek small letter epsilon,
	"equiv"-[8801],		% identical to, U+2261 ISOtech
	"eta"-[951],		% greek small letter eta, U+03B7 ISOgrk3
	"eth"-[240],		% latin small letter eth, U+00F0 ISOlat1>
	"euml"-[235],		% latin small letter e with diaeresis,
	"euro"-[8364],		% euro sign, U+20AC NEW
	"exist"-[8707],		% there exists, U+2203 ISOtech
	"fnof"-[402],		% latin small f with hook = function
	"forall"-[8704],	% for all, U+2200 ISOtech
	"frac12"-[189],		% vulgar fraction one half
	"frac14"-[188],		% vulgar fraction one quarter
	"frac34"-[190],		% vulgar fraction three quarters
	"frasl"-[8260],		% fraction slash, U+2044 NEW
	"gamma"-[947],		% greek small letter gamma,
	"ge"-[8805],		% greater-than or equal to,
	"hArr"-[8660],		% left right double arrow,
	"harr"-[8596],		% left right arrow, U+2194 ISOamsa
	"hearts"-[9829],	% black heart suit = valentine,
	"hellip"-[8230],	% horizontal ellipsis = three dot leader,
	"iacute"-[237],		% latin small letter i with acute,
	"icirc"-[238],		% latin small letter i with circumflex,
	"iexcl"-[161],		% inverted exclamation mark, U+00A1 ISOnum>
	"igrave"-[236],		% latin small letter i with grave,
	"image"-[8465],		% blackletter capital I = imaginary part,
	"infin"-[8734],		% infinity, U+221E ISOtech
	"int"-[8747],		% integral, U+222B ISOtech
	"iota"-[953],		% greek small letter iota, U+03B9 ISOgrk3
	"iquest"-[191],		% inverted question mark
	"isin"-[8712],		% element of, U+2208 ISOtech
	"iuml"-[239],		% latin small letter i with diaeresis,
	"kappa"-[954],		% greek small letter kappa,
	"lArr"-[8656],		% leftwards double arrow, U+21D0 ISOtech
	"lambda"-[955],		% greek small letter lambda,
	"lang"-[9001],		% left-pointing angle bracket = bra,
	"laquo"-[171],		% left-pointing double angle quotation mark
	"larr"-[8592],		% leftwards arrow, U+2190 ISOnum
	"lceil"-[8968],		% left ceiling = apl upstile,
	"ldquo"-[8220],		% left double quotation mark,
	"le"-[8804],		% less-than or equal to, U+2264 ISOtech
	"lfloor"-[8970],	% left floor = apl downstile,
	"lowast"-[8727],	% asterisk operator, U+2217 ISOtech
	"loz"-[9674],		% lozenge, U+25CA ISOpub
	"lrm"-[8206],		% left-to-right mark, U+200E NEW RFC 2070
	"lsaquo"-[8249],	% single left-pointing angle quotation mark,
	"lsquo"-[8216],		% left single quotation mark,
	"macr"-[175],		% macron = spacing macron = overline
	"mdash"-[8212],		% em dash, U+2014 ISOpub
	"micro"-[181],		% micro sign, U+00B5 ISOnum>
	"middot"-[183],		% middle dot = Georgian comma
	"minus"-[8722],		% minus sign, U+2212 ISOtech
	"mu"-[956],			% greek small letter mu, U+03BC ISOgrk3
	"nabla"-[8711],		% nabla = backward difference,
	"nbsp"-[160],		% no-break space = non-breaking space,
	"ndash"-[8211],		% en dash, U+2013 ISOpub
	"ne"-[8800],		% not equal to, U+2260 ISOtech
	"ni"-[8715],		% contains as member, U+220B ISOtech
	"not"-[172],		% not sign, U+00AC ISOnum>
	"notin"-[8713],		% not an element of, U+2209 ISOtech
	"nsub"-[8836],		% not a subset of, U+2284 ISOamsn
	"ntilde"-[241],		% latin small letter n with tilde,
	"nu"-[957],			% greek small letter nu, U+03BD ISOgrk3
	"oacute"-[243],		% latin small letter o with acute,
	"ocirc"-[244],		% latin small letter o with circumflex,
	"oelig"-[339],		% latin small ligature oe, U+0153 ISOlat2
	"ograve"-[242],		% latin small letter o with grave,
	"oline"-[8254],		% overline = spacing overscore,
	"omega"-[969],		% greek small letter omega,
	"omicron"-[959],	% greek small letter omicron, U+03BF NEW
	"oplus"-[8853],		% circled plus = direct sum,
	"or"-[8744],		% logical or = vee, U+2228 ISOtech
	"ordf"-[170],		% feminine ordinal indicator, U+00AA ISOnum>
	"ordm"-[186],		% masculine ordinal indicator,
	"oslash"-[248],		% latin small letter o with stroke,
	"otilde"-[245],		% latin small letter o with tilde,
	"otimes"-[8855],	% circled times = vector product,
	"ouml"-[246],		% latin small letter o with diaeresis,
	"para"-[182],		% pilcrow sign = paragraph sign,
	"part"-[8706],		% partial differential, U+2202 ISOtech
	"permil"-[8240],	% per mille sign, U+2030 ISOtech
	"perp"-[8869],		% up tack = orthogonal to = perpendicular,
	"phi"-[966],		% greek small letter phi, U+03C6 ISOgrk3
	"pi"-[960],			% greek small letter pi, U+03C0 ISOgrk3
	"piv"-[982],		% greek pi symbol, U+03D6 ISOgrk3
	"plusmn"-[177],		% plus-minus sign = plus-or-minus sign,
	"pound"-[163],		% pound sign, U+00A3 ISOnum>
	"prime"-[8242],		% prime = minutes = feet, U+2032 ISOtech
	"prod"-[8719],		% n-ary product = product sign,
	"prop"-[8733],		% proportional to, U+221D ISOtech
	"psi"-[968],		% greek small letter psi, U+03C8 ISOgrk3
	"rArr"-[8658],		% rightwards double arrow,
	"radic"-[8730],		% square root = radical sign,
	"rang"-[9002],		% right-pointing angle bracket = ket,
	"raquo"-[187],		% right-pointing double angle quotation mark
	"rarr"-[8594],		% rightwards arrow, U+2192 ISOnum
	"rceil"-[8969],		% right ceiling, U+2309 ISOamsc
	"rdquo"-[8221],		% right double quotation mark,
	"real"-[8476],		% blackletter capital R = real part symbol,
	"reg"-[174],		% registered sign = registered trade mark sign,
	"rfloor"-[8971],	% right floor, U+230B ISOamsc
	"rho"-[961],		% greek small letter rho, U+03C1 ISOgrk3
	"rlm"-[8207],		% right-to-left mark, U+200F NEW RFC 2070
	"rsaquo"-[8250],	% single right-pointing angle quotation mark,
	"rsquo"-[8217],		% right single quotation mark,
	"sbquo"-[8218],		% single low-9 quotation mark, U+201A NEW
	"scaron"-[353],		% latin small letter s with caron,
	"sdot"-[8901],		% dot operator, U+22C5 ISOamsb
	"sect"-[167],		% section sign, U+00A7 ISOnum>
	"shy"-[173],		% soft hyphen = discretionary hyphen,
	"sigma"-[963],		% greek small letter sigma,
	"sigmaf"-[962],		% greek small letter final sigma,
	"sim"-[8764],		% tilde operator = varies with = similar to,
	"spades"-[9824],	% black spade suit, U+2660 ISOpub
	"sub"-[8834],		% subset of, U+2282 ISOtech
	"sube"-[8838],		% subset of or equal to, U+2286 ISOtech
	"sum"-[8721],		% n-ary sumation, U+2211 ISOamsb
	"sup"-[8835],		% superset of, U+2283 ISOtech
	"sup1"-[185],		% superscript one = superscript digit one,
	"sup2"-[178],		% superscript two = superscript digit two
	"sup3"-[179],		% superscript three = superscript digit three
	"supe"-[8839],		% superset of or equal to,
	"szlig"-[223],		% latin small letter sharp s = ess-zed,
	"tau"-[964],		% greek small letter tau, U+03C4 ISOgrk3
	"there4"-[8756],	% therefore, U+2234 ISOtech
	"theta"-[952],		% greek small letter theta,
	"thetasym"-[977],	% greek small letter theta symbol,
	"thinsp"-[8201],	% thin space, U+2009 ISOpub
	"thorn"-[254],		% latin small letter thorn with,
	"tilde"-[732],		% small tilde, U+02DC ISOdia
	"times"-[215],		% multiplication sign, U+00D7 ISOnum>
	"trade"-[8482],		% trade mark sign, U+2122 ISOnum
	"uArr"-[8657],		% upwards double arrow, U+21D1 ISOamsa
	"uacute"-[250],		% latin small letter u with acute,
	"uarr"-[8593],		% upwards arrow, U+2191 ISOnum
	"ucirc"-[251],		% latin small letter u with circumflex,
	"ugrave"-[249],		% latin small letter u with grave,
	"uml"-[168],		% diaeresis = spacing diaeresis,
	"upsih"-[978],		% greek upsilon with hook symbol,
	"upsilon"-[965],	% greek small letter upsilon,
	"uuml"-[252],		% latin small letter u with diaeresis,
	"weierp"-[8472],	% script capital P = power set
	"xi"-[958],			% greek small letter xi, U+03BE ISOgrk3
	"yacute"-[253],		% latin small letter y with acute,
	"yen"-[165],		% yen sign = yuan sign, U+00A5 ISOnum>
	"yuml"-[255],		% latin small letter y with diaeresis,
	"zeta"-[950],		% greek small letter zeta, U+03B6 ISOgrk3
	"zwj"-[8205],		% zero width joiner, U+200D NEW RFC 2070
	"zwnj"-[8204]		% zero width non-joiner,
	] ).

/* @pred chars( ?Chars, ?Plus, ?Minus )
 *
 * used as chars( ?Chars ) in a DCG to
 * copy the list Chars inline.
 *
 * This is best expressed in terms of append/3 where append/3 is built-in.
 * For other Prologs, a straightforward specification can be used:
 *
 *	chars( [] ) --> "".
 *	chars( [Char|Chars] ) -->
 *		[Char],
 *		chars( Chars ).
 */

chars( Chars, Plus, Minus ) :-
	append( Chars, Minus, Plus ).


