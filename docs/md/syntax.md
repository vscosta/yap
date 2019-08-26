## [YAP Syntax](YAPSyntax)
 <!--- $0 --->

<!--- @{ --->
<!--- @ingroup --->

We will describe the syntax of YAP at two levels. We first will
describe the syntax for Prolog terms. In a second level we describe
the  tokens from which Prolog  terms are
built.

### [Syntax of Terms ](Formal_Syntax)
<!--- @{ --->
<!--- @ingroup --->

Below, we describe the syntax of YAP terms from the different
classes of tokens defined above. The formalism used will be <em>BNF</em>,
extended where necessary with attributes denoting integer precedence or
operator type.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 term       ---->     subterm(1200)   end_of_term_marker

 subterm(N) ---->     term(M)         [M <= N]

 term(N)    ---->     op(N, fx) subterm(N-1)
             |        op(N, fy) subterm(N)
             |        subterm(N-1) op(N, xfx) subterm(N-1)
             |        subterm(N-1) op(N, xfy) subterm(N)
             |        subterm(N) op(N, yfx) subterm(N-1)
             |        subterm(N-1) op(N, xf)
             |        subterm(N) op(N, yf)

 term(0)   ---->      atom '(' arguments ')'
             |        '(' subterm(1200)  ')'
             |        '{' subterm(1200)  '}'
             |        list
             |        string
             |        number
             |        atom
             |        variable

 arguments ---->      subterm(999)
             |        subterm(999) ',' arguments

 list      ---->      '[]'
             |        '[' list_expr ']'

 list_expr ---->      subterm(999)
             |        subterm(999) list_tail

 list_tail ---->      ',' list_expr
             |        ',..' subterm(999)
             |        '|' subterm(999)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Notes:

+ op(N,T) denotes an atom which has been previously declared with type _T_ and base precedence _N_.

+ Since `,` is itself a pre-declared operator with type `xfy` and precedence 1000, if a subterm starts with a '(',  op must be followed by a space to avoid ambiguity with the case of a functor followed by arguments, e.g.:

~~~~~
+ (a,b)        
~~~~~

is the same as `+(,(a,b))` of arity one; in contrast

~~~~
+(a,b)         
~~~~

is the same as `+(a,b)` of arity two.

+ In the first rule for term(0) no blank space should exist between atom and `(`.

+ Each term to be read by the YAP parser must end with a single dot, followed by a blank (in the sense mentioned in the previous paragraph). When a name consisting of a single dot could be taken for
the end of term marker, the ambiguity should be avoided by surrounding the dot with single quotes.

<!--- @} --->
### [Prolog Tokens ](Tokens)
 ]($0)

<!--- @{ --->
<!--- @ingroup --->

Prolog tokens are grouped into the following categories:

### [Numbers ](Numbers)
 ]($0)
<!--- @{ --->
<!--- @ingroup --->

Numbers can be further subdivided into integer and floating-point numbers.

#### [Integers ](Integers)
 ]($0)
<!--- @{ --->
<!--- @ingroup --->

Integer numbers
are described by the following regular expression:

~~~~

<integer> :=   {<digit>+<singlequote>|
	0{xXo}}<alpha_numeric_char>+

~~~~

where:

1. {...} stands for optionality, \a + optional repetition (one or more times),

2. \\\<digit\\\> denotes one of the characters 0 ... 9,

3. | denotes or,

4.  \\\<single-quote\\\> denotes the character "'".

The digits
before the \a \\\<single-quote\\\> character, when present, form the number
basis, that can go from 0, 1 and up to 36. Letters from `A` to
`Z` are used when the basis is larger than 10.

Note that if no basis is specified then base 10 is assumed. Note also
that the last digit of an integer token can not be immediately followed
by one of the characters 'e', 'E', or '.'.

Following the ISO standard, YAP also accepts directives of the
form `0x` to represent numbers in hexadecimal base and of the form
`0o` to represent numbers in octal base. For usefulness,
YAP also accepts directives of the form `0X` to represent
numbers in hexadecimal base.

##### Example:
the following tokens all denote the same integer

~~~~
10  2'1010  3'101  8'12  16'a  36'a  0xa  0o12
~~~~

Numbers of the form `0'a` are used to represent character
constants. So, the following tokens denote the same integer:

~~~~
0'd  100
~~~~

YAP (version 6.3.4) supports integers that can fit
the word size of the machine. This is 32 bits in most current machines,
but 64 in some others, such as the Alpha running Linux or Digital
Unix. The scanner will read larger or smaller integers erroneously.
](@})

#### [Floats](Floats)
 ]($0)
<!--- @} --->
<!--- @ingroup --->

Floating-point numbers are described by:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   <float> := <digit>+{<dot><digit>+}
               <exponent-marker>{<sign>}<digit>+
            |<digit>+<dot><digit>+
               {<exponent-marker>{<sign>}<digit>+}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

where

1. \\\<dot\\\> denotes the decimal-point character '.',

2. \\\<exponent-marker\\\> denotes one of 'e' or 'E', and

3. \\\<sign\\\> denotes
one of '+' or '-'.

##### [[Examples:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
10.0   10e3   10e-3   3.1415e+3
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Floating-point numbers are represented as a double in the target
machine. This is usually a 64-bit number.

](@})
](@})
### [[Character Strings ](Strings)
 ]($0)
<!--- @{ --->

Strings are described by the following rules:

~~~~
  string --> " string_quoted_characters "
  string --> ` string_quoted_characters `

  string_quoted_characters --> '"' '"' string_quoted_characters
  string_quoted_characters --> '\'
                          escape_sequence string_quoted_characters
  string_quoted_characters -->
                          string_character string_quoted_characters

  escape_sequence --> 'a' | 'b' | 'r' | 'f' | 't' | 'n' | 'v'
  escape_sequence --> '\' | '"' | ''' | '`'
  escape_sequence --> at_most_3_octal_digit_seq_char '\'
  escape_sequence --> 'x' at_most_2_hexa_digit_seq_char '\'
~~~~

where `string_character` is any character except the double quote (back quote)
and escape characters.

YAP supports four different textual elements:

 + Atoms, mentioned above, are textual representations of symbols, that are interned in the
 data-base. They are stored either in ISO-LATIN-1 (first 256 code points), or as UTF-32.

 + Strings are atomic representations of text. The back-quote character is used to identify these objects in the program. Strings exist as stack objects, in the same way as other Prolog terms. As Prolog unification cannot be used to manipulate strings, YAP includes built-ins such as string_arg/3, sub_string/5, or string_concat to manipulate them efficiently. Strings are stored as opaque objects containing a

 + Lists of codes represent text as a list of numbers, where each number is a character code. A string of _N_ bytes requires _N_ pairs, that is _2N_ cells, leading to a total of 16 bytes per character on 64 byte machines. Thus, they are a very expensive, but very flexible representation, as one can use unification to construct and access string elements.

 + Lists of atoms represent text as a list of atoms, where each number has a single character code. A string of _N_ bytes also requires _2N_ pairs. They have similar properties to lists of codes.

 The flags `double_quotes` and `backquoted_string` change the interpretation of text strings, they can take the
 values `atom`, `string`, `codes`, and `chars`.

Examples:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
""   "a string"   "a double-quote:"""
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The first string is an empty string, the last string shows the use of
double-quoting.

Escape sequences can be used to include the non-printable characters
`a` (alert), `b` (backspace), `r` (carriage return),
`f` (form feed), `t` (horizontal tabulation), `n` (new
line), and `v` (vertical tabulation). Escape sequences also be
include the meta-characters `\\`, `"`, `'`, and
`''`. Last, one can use escape sequences to include the characters
either as an octal or hexadecimal number.

The next examples demonstrates the use of escape sequences in YAP:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"\x0c\" "\01\" "\f" "\\"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The first three examples return a list including only character 12 (form
feed). The last example escapes the escape character.

Escape sequences were not available in C-Prolog and in original
versions of YAP up to 4.2.0. Escape sequences can be disabled by using:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- yap_flag(character_escapes,false).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



### [Atoms ](Atoms)

<!--- @ingroup --->

<!--- @{ --->

Atoms are defined by one of the following rules:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   atom --> solo-character
   atom --> lower-case-letter name-character*
   atom --> symbol-character+
   atom --> single-quote  single-quote
   atom --> ''' atom_quoted_characters '''

  atom_quoted_characters --> ''' ''' atom_quoted_characters
  atom_quoted_characters --> '\' atom_sequence string_quoted_characters
  atom_quoted_characters --> character string_quoted_characters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   <solo-character>     denotes one of:    ! ;
   <symbol-character>   denotes one of:    # & * + - . / : <
                                           = > ? @ \ ^ ~ `
   <lower-case-letter>  denotes one of:    a...z
   <name-character>     denotes one of:    _ a...z A...Z 0....9
   <single-quote>       denotes:           '
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and `string_character` denotes any character except the double quote
and escape characters. Note that escape sequences in strings and atoms
follow the same rules.

Examples:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a   a12x   '$a'   !   =>  '1 2'
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Version `4.2.0` of YAP removed the previous limit of 256
characters on an atom. Size of an atom is now only limited by the space
available in the system.

<!--- @} --->

### [Variables ](Variables)

<!--- @ingroup --->
<!--- @{ --->

Variables are described by:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   <variable-starter><variable-character>+
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  <variable-starter>   denotes one of:    _ A...Z
  <variable-character> denotes one of:    _ a...z A...Z
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If a variable is referred only once in a term, it needs not to be named
and one can use the character `_` to represent the variable. These
variables are known as anonymous variables. Please note that different
occurrences of `_` on the same term represent <em>different</em>
anonymous variables.

YAP accepts both the character `_`, or `_` followed by the usual
alphanumeric characters. In the former case, each `_` corresponds to a
different anonymous variable. In the latter case, a name can be used
to represent a singleton variable. This allows preserving the name of arguments across clauses, as in the following example:

~~~~~~
member(El, [El|_Tail]).
member(El, [_El|Tail]) :-
      member(El, Tail).
~~~~~~

In the example _El_ and _Tail_ refer to the head and tail of the clause
clause. Using the underscore, we can preserve the name while declaring
the variables are singletons.

<!--- @} --->
### [Punctuation Tokens ](Punctuation_Tokens)

<!--- @ingroup --->
<!--- @{ --->
Punctuation tokens consist of one of the following characters:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
( ) , [ ] { } |
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These characters are used to group terms.

<!--- @} --->
### [Character Layout ](LayoutComents)

<!--- @{ --->
<!--- @ingroup --->
Any characters with ASCII code less than or equal to 32 appearing before
a token are ignored.

All the text appearing in a line after the character \a % is taken to
be a comment and ignored (including \a %).  Comments can also be
inserted by using the sequence `/\*` to start the comment and
`\*` followed by `/` to finish it. In the presence of any sequence of comments or
layout characters, the YAP parser behaves as if it had found a
single blank character. The end of a file also counts as a blank
character for this purpose.

<!--- @} --->

### [Encoding Wide Character Support ](WideChars)

<!--- @ingroup --->
<!--- @{ --->


YAP now implements a SWI-Prolog compatible interface to wide
characters and the Universal Character Set (UCS). The following text
was adapted from the SWI-Prolog manual.

YAP now  supports wide characters, characters with character
codes above 255 that cannot be represented in a single byte.
<em>Universal Character Set</em> (UCS) is the ISO/IEC 10646 standard
that specifies a unique 31-bits unsigned integer for any character in
any language.  It is a superset of 16-bit Unicode, which in turn is
a superset of ISO 8859-1 (ISO Latin-1), a superset of US-ASCII.  UCS
can handle strings holding characters from multiple languages and
character classification (uppercase, lowercase, digit, etc.) and
operations such as case-conversion are unambiguously defined.

 YAP uses UTF-8 internally. In this encoding, the representation of
code points can have variable length. ASCII codes still take a single
byte, but western european latin text will have accented
characeters. that require two bytes in UTF-8. This representational
issue is completely transparent to the Prolog user.  Users of the
foreign language interface sometimes need to be aware of these issues
though. Notice that this will likely change in the future, we probably
will use an UTF-8 based representation.

YAP assumes that strings are null terminated. UTF-8 has no support for
encoding null characters within a sequence of codes, but YAP does
prove some limited support for an UTF extension that allows such
encodings.

Character coding comes into view when characters of strings need to be
read from or written to file or when they have to be communicated to
other software components using the foreign language interface. In this
section we only deal with I/O through streams, which includes file I/O
as well as I/O through network sockets.

<!--- @} --->
#### [Wide character encodings on streams ](Stream_Encoding)

<!--- @ingroup --->
<!--- @{ --->

The UCS standard describes all possible characters (or code points, as they include
ideograms, ligatures, and other symbols). The current version, Unicode 8.0, allows
code points up to 0x10FFFF, and thus allows for 1,114,112 code points. See [Unicode Charts](http://unicode.org/charts/) for the supported languages.

Notice that most symbols are rarely used. Encodings represent the Unicode characters in a way
that is more suited for communication. The most popular encoding, especially in the context of the web and in the Unix/Linux/BSD/Mac communities, is
UTF-8. UTF-8 is compact and as it uses bytes, does not have different endianesses.
Bytes 0...127 represent simply the corresponding US-ASCII
character, while bytes 128...255 are used for multi-byte
encoding of characters placed higher in the UCS space.

Especially on
MS-Windows and Java the 16-bit Unicode standard, represented by pairs of bytes is
also popular. Originally, Microsoft supported a UCS-2 with 16 bits that
 could represent only up to 64k characters. This was later extended to support the full
 Unicode, we will call the latter version UTF-16. The extension uses a hole in the first 64K code points. Characters above 0xFFFF are divided into two 2-byte words, each one in that hole. There are two versions of UTF-16: big and low
 endian. By default, UTF-16 is big endian, in practice most often it is used on Intel
 hardware that is naturally little endian.

 UTF-32, often called UCS-4, provides a natural interface where a code point is coded as
 four octets. Unfortunately, it is also more expensive, so it is not as widely used.

 Last, other encodings are also commonly used. One such legacy encoding is ISO-LATIN-1, that
 supported latin based languages in western europe. YAP currently uses either ISO-LATIN-1 or UTF-32
 internally.

Prolog supports the default encoding used by the Operating System,
Namely, YAP checks the variables LANG, LC_ALL and LC_TYPE. Say, if at boot YAP detects that the
environment variable `LANG` ends in "UTF-8", this encoding is
assumed. Otherwise, the default is `text` and the translation is
left to the wide-character functions of the C-library (note that the
Prolog native UTF-8 mode is considerably faster than the generic
`mbrtowc()` one).  

Prolog allows the encoding to be specified explicitly in
load_files/2 for loading Prolog source with an alternative
encoding, `open/4` when opening files or using `set_stream/2` on
any open stream (not yet implemented). For Prolog source files we also
provide the `encoding/1` directive that can be used to switch
between encodings that are compatible to US-ASCII (`ascii`,
`iso_latin_1`, `utf8` and many locales).

For
additional information and Unicode resources, please visit the
[unicode](http://www.unicode.org/) organization web page.

YAP currently defines and supports the following encodings:

  + `octet`
Default encoding for <em>binary</em> streams.  This causes
the stream to be read and written fully untranslated.

  + `ascii` or `US_ASCII`
7-bit encoding in 8-bit bytes.  Equivalent to `iso_latin_1`,
but generates errors and warnings on encountering values above
127.

  + `iso_latin_1` or `ISO-8859-1`
8-bit encoding supporting many western languages.  This causes
the stream to be read and written fully untranslated.

  + `text`
C-library default locale encoding for text files.  Files are read and
written using the C-library functions `mbrtowc()` and
`wcrtomb()`.  This may be the same as one of the other locales,
notably it may be the same as `iso_latin_1` for western
languages and `utf8` in a UTF-8 context.

  + `utf8`, `iso_utf8`, or `UTF-8``
Multi-byte encoding of the full Unicode 8, compatible to `ascii` .
See above.

  + `unicode_be` or `UCS-2BE`
Unicode Big Endian.  Reads input in pairs of bytes, most
significant byte first.  Can only represent 16-bit characters.

  + `unicode_le` or `UCS-2LE`
Unicode Little Endian.  Reads input in pairs of bytes, least
significant byte first.  Can only represent 16-bit characters.

  + `utf16_le` or `UTF-16LE` (experimental)
  UTF-16 Little Endian.  Reads input in pairs of bytes, least
significant byte first.  Can  represent the  full Unicode.

  + `utf16_le` or `UTF-16BE` (experimental)
Unicode Big Endian.  Reads input in pairs of bytes, least
significant byte first.  Can  represent the  full Unicode.

  + `utf32_le` or `UTF-32LE` (experimental)
  UTF-16 Little Endian.  Reads input in pairs of bytes, least
significant byte first.  Can  represent the  full Unicode.

  + `utf32_le` or `UTF-32BE` (experimental)
Unicode Big Endian.  Reads input in pairs of bytes, least
significant byte first.  Can only represent 16-bit characters.


Note that not all encodings can represent all characters. This implies
that writing text to a stream may cause errors because the stream
cannot represent these characters. The behaviour of a stream on these
errors can be controlled using `open/4` or `set_stream/2` (not
implemented). Initially the terminal stream write the characters using
Prolog escape sequences while other streams generate an I/O exception.

<!--- @} --->

##### [BOM: Byte Order Mark ](BOM)

<!--- @ingroup --->
<!--- @{ --->

From Stream Encoding, you may have got the impression that
text-files are complicated. This section deals with a related topic,
making live often easier for the user, but providing another worry to
the programmer.   *BOM* or <em>Byte Order Marker</em> is a technique
for identifying Unicode text-files as well as the encoding they
use. Please read the [W3C](https://www.w3.org/International/questions/qa-byte-order-mark.en.php]
page for a detailed explanation of byte-order marks.

BOMa are necessary on multi-byte encodings, such as UTF-16 and UTF-32. There is  a BOM for UTF-8, but it is rarely used.
The BOM is handled by the open/4 predicate. By default, text-files are
probed for the BOM when opened for reading. If a BOM is found, the
encoding is set accordingly and the property `bom(true)` is
available through stream_property/2. When opening a file for
writing, writing a BOM can be requested using the option
`bom(true)` with `open/4`. YAP will parse an UTF-8 file for a BOM only if explicitly required to do so. Do notice that YAP will write a BOM by default on UTF-16 (including UCS-2) and
UTF-32; otherwise the default is not to write a BOM. BOMs are not avaliable for ASCII and
ISO-LATIN-1.

### [Summary of YAP Predefined Operators ](Operators)

<!--- @{ --->
<!--- @ingroup --->

The Prolog syntax caters for operators of three main kinds:

  +  prefix;
  + infix;
  + postfix.


Each operator has precedence in the range 1 to 1200, and this
precedence is used to disambiguate expressions where the structure of the
term denoted is not made explicit using brackets. The operator of higher
precedence is the main functor.

If there are two operators with the highest precedence, the ambiguity
is solved analyzing the types of the operators. The possible infix types are:
 _xfx_,  _xfy_, and  _yfx_.

With an operator of type  _xfx_ both sub-expressions must have lower
precedence than the operator itself, unless they are bracketed (which
assigns to them zero precedence). With an operator type  _xfy_ only the  
left-hand sub-expression must have lower precedence. The opposite happens
for  _yfx_ type.

A prefix operator can be of type  _fx_ or  _fy_.
A postfix operator can be of type  _xf_ or  _yf_.
The meaning of the notation is analogous to the above.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a + b * c
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
means

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a + (b * c)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as + and \* have the following types and precedences:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:-op(500,yfx,'+').
:-op(400,yfx,'*').
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now defining

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:-op(700,xfy,'++').
:-op(700,xfx,'=:=').
a ++ b =:= c
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 means

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a ++ (b =:= c)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following is the list of the declarations of the predefined operators:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:-op(1200,fx,['?-', ':-']).
:-op(1200,xfx,[':-','-->']).
:-op(1150,fx,[block,dynamic,mode,public,multifile,meta_predicate,
              sequential,table,initialization]).
:-op(1100,xfy,[';','|']).
:-op(1050,xfy,->).
:-op(1000,xfy,',').
:-op(999,xfy,'.').
:-op(900,fy,['\+', not]).
:-op(900,fx,[nospy, spy]).
:-op(700,xfx,[@>=,@=<,@<,@>,<,=,>,=:=,=\=,\==,>=,=<,==,\=,=..,is]).
:-op(500,yfx,['\/','/\','+','-']).
:-op(500,fx,['+','-']).
:-op(400,yfx,['<<','>>','//','*','/']).
:-op(300,xfx,mod).
:-op(200,xfy,['^','**']).
:-op(50,xfx,same).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

<!--- @} --->
