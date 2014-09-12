@defgroup Syntax YAP Syntax
@ingroup YAPProgramming
@{

We will describe the syntax of YAP at two levels. We first will
describe the syntax for Prolog terms. In a second level we describe
the \a tokens from which Prolog \a terms are
built.

@section Formal_Syntax Syntax of Terms

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

   + \a op(N,T) denotes an atom which has been previously declared with type
      \a T and base precedence \a N.

  + Since ',' is itself a pre-declared operator with type \a xfy and
       precedence 1000, is \a subterm starts with a '(', \a op must be
       followed by a space to avoid ambiguity with the case of a functor
       followed by arguments, e.g.:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
+ (a,b)        [the same as '+'(','(a,b)) of arity one]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      versus

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
+(a,b)         [the same as '+'(a,b) of arity two]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  + 
In the first rule for term(0) no blank space should exist between
\a atom and '('.

  + 
Each term to be read by the YAP parser must end with a single
dot, followed by a blank (in the sense mentioned in the previous
paragraph). When a name consisting of a single dot could be taken for
the end of term marker, the ambiguity should be avoided by surrounding the
dot with single quotes.



@section Tokens Prolog Tokens

Prolog tokens are grouped into the following categories:

@subsection Numbers Numbers

Numbers can be further subdivided into integer and floating-point numbers.

@subsubsection Integers

Integer numbers
are described by the following regular expression:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

<integer> := {<digit>+<single-quote>|0{xXo}}<alpha_numeric_char>+

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

where {...} stands for optionality, \a + optional repetition (one or
more times), \a \\\<digit\\\> denotes one of the characters 0 ... 9, \a |
denotes or, and \a \\\<single-quote\\\> denotes the character "'". The digits
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

Example:
the following tokens all denote the same integer

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
10  2'1010  3'101  8'12  16'a  36'a  0xa  0o12
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Numbers of the form `0'a` are used to represent character
constants. So, the following tokens denote the same integer:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
0'd  100
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

YAP (version 6.3.4) supports integers that can fit
the word size of the machine. This is 32 bits in most current machines,
but 64 in some others, such as the Alpha running Linux or Digital
Unix. The scanner will read larger or smaller integers erroneously.

@subsubsection Floats

Floating-point numbers are described by:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   <float> := <digit>+{<dot><digit>+}
               <exponent-marker>{<sign>}<digit>+
            |<digit>+<dot><digit>+
               {<exponent-marker>{<sign>}<digit>+}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

where \a \\\<dot\\\> denotes the decimal-point character '.',
\a \\\<exponent-marker\\\> denotes one of 'e' or 'E', and \a \\\<sign\\\> denotes
one of '+' or '-'.

Examples:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
10.0   10e3   10e-3   3.1415e+3
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Floating-point numbers are represented as a double in the target
machine. This is usually a 64-bit number.

@subsection Strings Character Strings

Strings are described by the following rules:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  string --> '"' string_quoted_characters '"'

  string_quoted_characters --> '"' '"' string_quoted_characters
  string_quoted_characters --> '\'
                          escape_sequence string_quoted_characters
  string_quoted_characters -->
                          string_character string_quoted_characters

  escape_sequence --> 'a' | 'b' | 'r' | 'f' | 't' | 'n' | 'v'
  escape_sequence --> '\' | '"' | ''' | '`'
  escape_sequence --> at_most_3_octal_digit_seq_char '\'
  escape_sequence --> 'x' at_most_2_hexa_digit_seq_char '\'
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where `string_character` in any character except the double quote
and escape characters.

Examples:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
""   "a string"   "a double-quote:""" 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The first string is an empty string, the last string shows the use of
double-quoting. The implementation of YAP represents strings as
lists of integers. Since YAP 4.3.0 there is no static limit on string
size.

Escape sequences can be used to include the non-printable characters
`a` (alert), `b` (backspace), `r` (carriage return),
`f` (form feed), `t` (horizontal tabulation), `n` (new
line), and `v` (vertical tabulation). Escape sequences also be
include the meta-characters `\\`, `"`, `'`, and
```. Last, one can use escape sequences to include the characters
either as an octal or hexadecimal number.

The next examples demonstrates the use of escape sequences in YAP:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"\x0c\" "\01\" "\f" "\\" 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The first three examples return a list including only character 12 (form
feed). The last example escapes the escape character.

Escape sequences were not available in C-Prolog and in original
versions of YAP up to 4.2.0. Escape sequences can be disable by using:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- yap_flag(character_escapes,false).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection Atoms Atoms

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

@subsection Variables Variables

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
variables are known as anonymous variables. Note that different
occurrences of `_` on the same term represent <em>different</em>
anonymous variables. 

@subsection Punctuation_Tokens Punctuation Tokens

Punctuation tokens consist of one of the following characters:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
( ) , [ ] { } |
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These characters are used to group terms.

@subsection Layout Layout
Any characters with ASCII code less than or equal to 32 appearing before
a token are ignored.

All the text appearing in a line after the character \a % is taken to
be a comment and ignored (including \a %).  Comments can also be
inserted by using the sequence `/\*` to start the comment and
`\*` followed by `/` to finish it. In the presence of any sequence of comments or
layout characters, the YAP parser behaves as if it had found a
single blank character. The end of a file also counts as a blank
character for this purpose.

@section Encoding Wide Character Support

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

For this reason YAP, following SWI-Prolog, has two representations for
atoms. If the text fits in ISO Latin-1, it is represented as an array
of 8-bit characters.  Otherwise the text is represented as an array of
wide chars, which may take 16 or 32 bits.  This representational issue
is completely transparent to the Prolog user.  Users of the foreign
language interface sometimes need to be aware of these issues though.

Character coding comes into view when characters of strings need to be
read from or written to file or when they have to be communicated to
other software components using the foreign language interface. In this
section we only deal with I/O through streams, which includes file I/O
as well as I/O through network sockets.

@subsection Stream_Encoding Wide character encodings on streams

Although characters are uniquely coded using the UCS standard
internally, streams and files are byte (8-bit) oriented and there are a
variety of ways to represent the larger UCS codes in an 8-bit octet
stream. The most popular one, especially in the context of the web, is
UTF-8. Bytes 0...127 represent simply the corresponding US-ASCII
character, while bytes 128...255 are used for multi-byte
encoding of characters placed higher in the UCS space. Especially on
MS-Windows the 16-bit Unicode standard, represented by pairs of bytes is
also popular.

Prolog I/O streams have a property called <em>encoding</em> which
specifies the used encoding that influence `get_code/2` and
`put_code/2` as well as all the other text I/O predicates.

The default encoding for files is derived from the Prolog flag
`encoding`, which is initialised from the environment.  If the
environment variable `LANG` ends in "UTF-8", this encoding is
assumed. Otherwise the default is `text` and the translation is
left to the wide-character functions of the C-library (note that the
Prolog native UTF-8 mode is considerably faster than the generic
`mbrtowc()` one).  The encoding can be specified explicitly in
load_files/2 for loading Prolog source with an alternative
encoding, `open/4` when opening files or using `set_stream/2` on
any open stream (not yet implemented). For Prolog source files we also
provide the `encoding/1` directive that can be used to switch
between encodings that are compatible to US-ASCII (`ascii`,
`iso_latin_1`, `utf8` and many locales).  



For
additional information and Unicode resources, please visit
<http://www.unicode.org/>.

YAP currently defines and supports the following encodings:

  + octet
Default encoding for <em>binary</em> streams.  This causes
the stream to be read and written fully untranslated.

  + ascii
7-bit encoding in 8-bit bytes.  Equivalent to `iso_latin_1`,
but generates errors and warnings on encountering values above
127.

  + iso_latin_1
8-bit encoding supporting many western languages.  This causes
the stream to be read and written fully untranslated.

  + text
C-library default locale encoding for text files.  Files are read and
written using the C-library functions `mbrtowc()` and
`wcrtomb()`.  This may be the same as one of the other locales,
notably it may be the same as `iso_latin_1` for western
languages and `utf8` in a UTF-8 context.

  + utf8
Multi-byte encoding of full UCS, compatible to `ascii`.
See above.

  + unicode_be
Unicode Big Endian.  Reads input in pairs of bytes, most
significant byte first.  Can only represent 16-bit characters.

  + unicode_le
Unicode Little Endian.  Reads input in pairs of bytes, least
significant byte first.  Can only represent 16-bit characters.


Note that not all encodings can represent all characters. This implies
that writing text to a stream may cause errors because the stream
cannot represent these characters. The behaviour of a stream on these
errors can be controlled using `open/4` or `set_stream/2` (not
implemented). Initially the terminal stream write the characters using
Prolog escape sequences while other streams generate an I/O exception.

@subsection BOM BOM: Byte Order Mark

From Stream Encoding, you may have got the impression that
text-files are complicated. This section deals with a related topic,
making live often easier for the user, but providing another worry to
the programmer.   *BOM* or <em>Byte Order Marker</em> is a technique
for identifying Unicode text-files as well as the encoding they
use. Such files start with the Unicode character `0xFEFF`, a
non-breaking, zero-width space character. This is a pretty unique
sequence that is not likely to be the start of a non-Unicode file and
uniquely distinguishes the various Unicode file formats. As it is a
zero-width blank, it even doesn't produce any output. This solves all
problems, or ...

Some formats start of as US-ASCII and may contain some encoding mark to
switch to UTF-8, such as the `encoding="UTF-8"` in an XML header.
Such formats often explicitly forbid the the use of a UTF-8 BOM. In
other cases there is additional information telling the encoding making
the use of a BOM redundant or even illegal.

The BOM is handled by the `open/4` predicate. By default, text-files are
probed for the BOM when opened for reading. If a BOM is found, the
encoding is set accordingly and the property `bom(true)` is
available through stream_property/2. When opening a file for
writing, writing a BOM can be requested using the option
`bom(true)` with `open/4`.

@subsection Operators Summary of YAP Predefined Operators

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

@}
