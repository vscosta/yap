/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2003	 *
*									 *
**************************************************************************
*									 *
* File:		%W% %G%						         *
* Last rev:	22-1-03							 *
* mods:									 *
* comments:	Prolog's scanner					 *
*									 *
*************************************************************************/

/*
 * Description: 
 *
 * This module produces a list of tokens for use by the parser. The calling
 * program should supply a routine int nextch(charpos) int *charpos; which,
 * when called should produce the next char or -1 if none availlable. The
 * scanner will stop producing tokens when it either finds an end of file
 * (-1) or a token consisting of just '.' followed by a blank or control
 * char. Scanner errors will be signalled by the scanner exiting with a non-
 * zero  ErrorMsg and ErrorPos. Note that, even in this case, the scanner
 * will try to find the end of the term. A function char
 * *AllocScannerMemory(nbytes) should be supplied for allocating (temporary)
 * space for strings and for the table of prolog variables occurring in the
 * term. 
 *
 */

/**

@defgroup Formal_Syntax Syntax of Terms
@ingroup Syntax
@{


Prolog tokens are grouped into the following categories:

+ Numbers

Numbers can be further subdivided into integer and floating-point numbers.

  + Integers

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

  + Floats

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

+ Strings Character Strings

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

+ Atoms Atoms

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

+ Variables Variables

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

+ Punctuation Tokens

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

+ Encoding Wide Character Support

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

  + Stream_Encoding Wide character encodings on streams

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

    + BOM BOM: Byte Order Mark

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

			       */


#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "SWI-Stream.h"
#include "yapio.h"
#include "alloc.h"
#include "eval.h"
/* stuff we want to use in standard YAP code */
#include "pl-shared.h"
#include "pl-read.h"
#include "YapText.h"
#if _MSC_VER || defined(__MINGW32__) 
#if HAVE_FINITE==1
#undef HAVE_FINITE
#endif
#include <windows.h>
#endif
#include "iopreds.h"
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_WCTYPE_H
#include <wctype.h>
#endif
#if O_LOCALE
#include "locale.h"
#endif

/* You just can't trust some machines */
#define my_isxdigit(C,SU,SL)	(chtype(C) == NU || (C >= 'A' &&	\
				 C <= (SU)) || (C >= 'a' && C <= (SL)))
#define my_isupper(C)	( C >= 'A' && C <= 'Z' )
#define my_islower(C)	( C >= 'a' && C <= 'z' )

static Term float_send(char *, int);
static Term get_num(int *, int *, IOSTREAM *,char *,UInt,int);

/* token table with some help from Richard O'Keefe's PD scanner */
static char chtype0[NUMBER_OF_CHARS+1] =
{
EF,
/* nul soh stx etx eot enq ack bel  bs  ht  nl  vt  np  cr  so  si */
  BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,

/* dle dc1 dc2 dc3 dc4 nak syn etb can  em sub esc  fs  gs  rs  us */
  BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,

/* sp   !   "   #   $   %   &   '   (   )   *   +   ,   -   .   / */
  BS, SL, DC, SY, LC, CC, SY, QT, BK, BK, SY, SY, BK, SY, SY, SY,

/* 0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ? */
  NU, NU, NU, NU, NU, NU, NU, NU, NU, NU, SY, SL, SY, SY, SY, SY,

/* @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O */
  SY, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC,

/* P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _ */
  UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, BK, SY, BK, SY, UL,

/* `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o */
  SY, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,

/* p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~ del */
  LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, BK, BK, BK, SY, BS,

/* 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 */
  BS, BS,  BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,

/* 144 145    147 148 149 150 151 152 153 154 155 156 157 158 159 */
   BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,

/*     ¡   ¢   £   ¤   ¥   ¦   §   ¨   ©   ª   «   ¬   ­   ®   ¯   */
   BS, SY, SY, SY, SY, SY, SY, SY, SY, SY, LC, SY, SY, SY, SY, SY,

/* °   ±   ²   ³   ´   µ   ¶   ·   ¸   ¹   º   »   ¼   ½   ¾   ¿   */
   SY, SY, LC, LC, SY, SY, SY, SY, SY, LC, LC, SY, SY, SY, SY, SY,

/* À   Á   Â   Ã   Ä   Å   Æ   Ç   È   É   Ê   Ë   Ì   Í   Î   Ï    */
   UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC,

/* Ð   Ñ   Ò   Ó   Ô   Õ   Ö   ×   Ø   Ù   Ú   Û   Ü   Ý   Þ   ß    */
#ifdef  vms
   UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, LC,
#else
   UC, UC, UC, UC, UC, UC, UC, SY, UC, UC, UC, UC, UC, UC, UC, LC,
#endif
/* à   á   â   ã   ä   å   æ   ç   è   é   ê   ë   ì   í   î   ï    */
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,

/* ð   ñ   ò   ó   ô   õ   ö   ÷   ø   ù   ú   û   ü   cannot write the last three because of lcc    */
#ifdef  vms
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC
#else
   LC, LC, LC, LC, LC, LC, LC, SY, LC, LC, LC, LC, LC, LC, LC, LC
#endif
};


char *Yap_chtype = chtype0+1;

int
Yap_wide_chtype(Int ch) {
#if HAVE_WCTYPE_H
  if (iswalnum(ch)) {
    if (iswlower(ch)) return LC;
    if (iswdigit(ch)) return NU;
    return UC;
  }
  if (iswpunct(ch)) return SY;
#endif
  return BS;
}


static inline int
getchr__(IOSTREAM *inp)
{ int c = Sgetcode(inp);

  if ( !CharConversionTable || c < 0 || c >= 256 )
    return c;

  return CharConversionTable[c];
}


#define getchr(inp)  getchr__(inp)
#define getchrq(inp) Sgetcode(inp)

EXTERN inline int
GetCurInpPos (IOSTREAM *inp_stream)
{
  return inp_stream->posbuf.lineno;
}



/* in case there is an overflow */
typedef struct scanner_extra_alloc {
  struct scanner_extra_alloc *next;
  void *filler;
} ScannerExtraBlock;

static char *
AllocScannerMemory(unsigned int size)
{
  CACHE_REGS
  char *AuxSpScan;

  AuxSpScan = LOCAL_ScannerStack;
  size = AdjustSize(size);
  if (LOCAL_ScannerExtraBlocks) {
    struct scanner_extra_alloc *ptr;

    if (!(ptr = (struct scanner_extra_alloc *)malloc(size+sizeof(ScannerExtraBlock)))) {
      return NULL;
    }
    ptr->next = LOCAL_ScannerExtraBlocks;
    LOCAL_ScannerExtraBlocks = ptr;
    return (char *)(ptr+1);
  } else if (LOCAL_TrailTop <= AuxSpScan+size) {
    UInt alloc_size = sizeof(CELL) * K16;
 
    if (size > alloc_size)
      alloc_size = size;
    if(!Yap_growtrail(alloc_size, TRUE)) {
      struct scanner_extra_alloc *ptr;

      if (!(ptr = (struct scanner_extra_alloc *)malloc(size+sizeof(ScannerExtraBlock)))) {
	return NULL;
      }
      ptr->next = LOCAL_ScannerExtraBlocks;
      LOCAL_ScannerExtraBlocks = ptr;
      return (char *)(ptr+1);
    }
  }
  LOCAL_ScannerStack = AuxSpScan+size;
  return AuxSpScan;
}

static void
PopScannerMemory(char *block, unsigned int size)
{
  CACHE_REGS
  if (block == LOCAL_ScannerStack-size) {
    LOCAL_ScannerStack -= size;
  } else if (block == (char *)(LOCAL_ScannerExtraBlocks+1)) {
    struct scanner_extra_alloc *ptr = LOCAL_ScannerExtraBlocks;

    LOCAL_ScannerExtraBlocks = ptr->next;
    free(ptr);
  }
}

char *
Yap_AllocScannerMemory(unsigned int size)
{
  /* I assume memory has been initialised */
  return AllocScannerMemory(size);
}

extern double atof(const char *);

static Term
float_send(char *s, int sign)
{
  GET_LD
    Float f = (Float)(sign*atof(s));
#if HAVE_ISFINITE || defined(isfinite)
  if (truePrologFlag(PLFLAG_ISO)) { /* iso */
    if (!isfinite(f)) {
      LOCAL_ErrorMessage = "Float overflow while scanning";
      return(MkEvalFl(f));
    }
  }
#elif HAVE_FINITE
  if (truePrologFlag(PLFLAG_ISO)) { /* iso */
    if (!finite(f)) {
      LOCAL_ErrorMessage = "Float overflow while scanning";
      return(MkEvalFl(f));
    }
  }
#endif
  return (MkEvalFl(f));
}

/* we have an overflow at s */
static Term
read_int_overflow(const char *s, Int base, Int val, int sign)
{
#ifdef USE_GMP
  /* try to scan it as a bignum */
  mpz_t new;
  Term t;

  mpz_init_set_str (new, s, base);
  if (sign < 0)
    mpz_neg(new, new);
  t = Yap_MkBigIntTerm(new);
  mpz_clear(new);
  return t;
#else
  CACHE_REGS
  /* try to scan it as a float */
  return MkIntegerTerm(val);
#endif    
}

static int
send_error_message(char s[])
{
  CACHE_REGS
  LOCAL_ErrorMessage = s;
  return 0;
}

static wchar_t
read_quoted_char(int *scan_nextp, IOSTREAM *inp_stream)
{
  GET_LD
  int ch;

  /* escape sequence */
 do_switch:
  ch = getchrq(inp_stream);
  switch (ch) {
  case 10:
    return 0;
  case '\\':
    return '\\';
  case 'a':
    return '\a';
  case 'b':
    return '\b';
  case 'c':
    while (chtype((ch = getchrq(inp_stream))) == BS);
    {
      if (ch == '\\') {
	goto do_switch; 
      }
      return ch;
    }
  case 'd':
    return 127;
  case 'e':
    return '\x1B';  /* <ESC>, a.k.a. \e */
  case 'f':
    return '\f';
  case 'n':
    return '\n';
  case 'r':
    return '\r';
  case 's':         /* space */
    return ' ';
  case 't':
    return '\t';
  case 'u':
    {
      int i;
      wchar_t wc='\0';

      for (i=0; i< 4; i++) {
	ch = getchrq(inp_stream);
	if (ch>='0' && ch <= '9') {
	  wc += (ch-'0')<<((3-i)*4);
	} else if (ch>='a' && ch <= 'f') {
	  wc += ((ch-'a')+10)<<((3-i)*4);
	} else if (ch>='A' && ch <= 'F') {
	  wc += ((ch-'A')+10)<<((3-i)*4);
	} else {
	  return send_error_message("invalid escape sequence");
	}
      }
      return wc;
    }
  case 'U':
    {
      int i;
      wchar_t wc='\0';

      for (i=0; i< 8; i++) {
	ch = getchrq(inp_stream);
	if (ch>='0' && ch <= '9') {
	  wc += (ch-'0')<<((7-i)*4);
	} else if (ch>='a' && ch <= 'f') {
	  wc += ((ch-'a')+10)<<((7-i)*4);
	} else if (ch>='A' && ch <= 'F') {
	  wc += ((ch-'A')+10)<<((7-i)*4);
	} else {
	  return send_error_message("invalid escape sequence");
	}
      }
      return wc;
    }
  case 'v':
    return '\v';
  case 'z':         /* Prolog end-of-file */
    return send_error_message("invalid escape sequence \\z");
  case '\'':
    return '\'';
  case '"':
    return '"';
  case '`':
    return '`';
  case '^':
    if (truePrologFlag(PLFLAG_ISO)) {
      return send_error_message("invalid escape sequence");
    } else {
      ch = getchrq(inp_stream);
      if (ch ==  '?') {/* delete character */
	return 127;
      } else if (ch >= 'a' && ch < 'z') {/* hexa */
	return ch - 'a';
      } else if (ch >= 'A' && ch < 'Z') {/* hexa */
	return ch - 'A';
      } else {
	 return '^';
      }
    }
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
    /* character in octal: maximum of 3 digits, terminates with \ */
    /* follow ISO */
    {
      unsigned char so_far = ch-'0';
      ch = getchrq(inp_stream);
      if (ch >= '0' && ch < '8') {/* octal */
	so_far = so_far*8+(ch-'0');
	ch = getchrq(inp_stream);
	if (ch >= '0' && ch < '8') { /* octal */
	  so_far = so_far*8+(ch-'0');
	  ch = getchrq(inp_stream);
	  if (ch != '\\') {
	    return send_error_message("invalid octal escape sequence");
	  }
	  return so_far;
	} else if (ch == '\\') {
	  return so_far;
	} else {
	  return send_error_message("invalid octal escape sequence");
	}
      } else if (ch == '\\') {
	return so_far;
      } else {
	return send_error_message("invalid octal escape sequence");
      }
    }
  case 'x':
    /* hexadecimal character (YAP allows empty hexadecimal  */
    {
      unsigned char so_far = 0; 
      ch = getchrq(inp_stream);
      if (my_isxdigit(ch,'f','F')) {/* hexa */
	so_far = so_far * 16 + (chtype(ch) == NU ? ch - '0' :
				(my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
	ch = getchrq(inp_stream);
	if (my_isxdigit(ch,'f','F')) { /* hexa */
	  so_far = so_far * 16 + (chtype(ch) == NU ? ch - '0' :
				  (my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
	  ch = getchrq(inp_stream);
	  if (ch == '\\') {
	    return so_far;
	  } else {
	    return send_error_message("invalid hexadecimal escape sequence");
	  }
	} else if (ch == '\\') {
	  return so_far;
	} else {
	  return send_error_message("invalid hexadecimal escape sequence");
	} 
      } else if (ch == '\\') {
	return so_far;
      } else {
	return send_error_message("invalid hexadecimal escape sequence");
      }
    }
  default:
    /* accept sequence. Note that the ISO standard does not
       consider this sequence legal, whereas SICStus would
       eat up the escape sequence. */
    return send_error_message("invalid escape sequence");
  }
}
	    
static int
num_send_error_message(char s[])
{
  CACHE_REGS
  LOCAL_ErrorMessage = s;
  return TermNil;
}

/* reads a number, either integer or float */


static Term
get_num(int *chp, int *chbuffp, IOSTREAM *inp_stream, char *s, UInt max_size, int sign)
{
  GET_LD
  char *sp = s;
  int ch = *chp;
  Int val = 0L, base = ch - '0';
  int might_be_float = TRUE, has_overflow = FALSE;
  const unsigned char *decimalpoint;

  *sp++ = ch;
  ch = getchr(inp_stream);
  /*
   * because of things like 00'2, 03'2 and even better 12'2, I need to
   * do this (have mercy) 
   */
  if (chtype(ch) == NU) {
    *sp++ = ch;
    if (--max_size == 0) {
      return num_send_error_message("Number Too Long");
    }
    base = 10 * base + ch - '0';
    ch = getchr(inp_stream);
  }
  if (ch == '\'') {
    if (base > 36) {
      return num_send_error_message("Admissible bases are 0..36");
    }
    might_be_float = FALSE;
    if (--max_size == 0) {
      return num_send_error_message("Number Too Long");
    }
    *sp++ = ch;
    ch = getchr(inp_stream);
    if (base == 0) {
      wchar_t ascii = ch;
      int scan_extra = TRUE;

      if (ch == '\\' &&
	  Yap_GetModuleEntry(CurrentModule)->flags & M_CHARESCAPE) {
	ascii = read_quoted_char(&scan_extra, inp_stream);
      }
      /* a quick way to represent ASCII */
      if (scan_extra)
	*chp = getchr(inp_stream);
      if (sign == -1) {
	return MkIntegerTerm(-ascii);
      }
      return MkIntegerTerm(ascii);
    } else if (base >= 10 && base <= 36) {
      int upper_case = 'A' - 11 + base;
      int lower_case = 'a' - 11 + base;

      while (my_isxdigit(ch, upper_case, lower_case)) {
	Int oval = val;
	int chval = (chtype(ch) == NU ? ch - '0' :
		     (my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
	if (--max_size == 0) {
	  return num_send_error_message("Number Too Long");
	}
	*sp++ = ch;
	val = oval * base + chval;
	if (oval != (val-chval)/base) /* overflow */
	  has_overflow = (has_overflow || TRUE);
	ch = getchr(inp_stream);
      }
    }
  } else if (ch == 'x' && base == 0) {
    might_be_float = FALSE;
    if (--max_size == 0) {
      return num_send_error_message("Number Too Long");
    }
    *sp++ = ch;
    ch = getchr(inp_stream);
    while (my_isxdigit(ch, 'F', 'f')) {
      Int oval = val;
      int chval = (chtype(ch) == NU ? ch - '0' :
		   (my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
      if (--max_size == 0) {
	return num_send_error_message("Number Too Long");
      }
      *sp++ = ch;
      val = val * 16 + chval;
      if (oval != (val-chval)/16) /* overflow */
	has_overflow = TRUE;
      ch = getchr(inp_stream);
    }
    *chp = ch;
  }
  else if (ch == 'o' && base == 0) {
    might_be_float = FALSE;
    base = 8;
    ch = getchr(inp_stream);
  } else if (ch == 'b' && base == 0) {
    might_be_float = FALSE;
    base = 2;
    ch = getchr(inp_stream);
  } else {
    val = base;
    base = 10;
  }
  while (chtype(ch) == NU) {
    Int oval = val;
    if (!(val == 0 && ch == '0') || has_overflow) {
      if (--max_size == 0) {
	return num_send_error_message("Number Too Long");
      }
      *sp++ = ch;
    }
    if (ch - '0' >= base) {
      if (sign == -1)
	return MkIntegerTerm(-val);
      return MkIntegerTerm(val);
    }
    val = val * base + ch - '0';
    if (val/base != oval || val -oval*base != ch-'0') /* overflow */
      has_overflow = TRUE;
    ch = getchr(inp_stream);
  }
  if (might_be_float && ( ch == '.'  || ch == 'e' || ch == 'E')) {
    int has_dot = ( ch == '.' );
    if (has_dot) {
      unsigned char * dp;
      int dc;

      if (chtype(ch = getchr(inp_stream)) != NU) {
	if ( ch == 'e' || ch == 'E') {
	  if (truePrologFlag(PLFLAG_ISO))
	    return num_send_error_message("Float format not allowed in ISO mode");
	} else {/* followed by a letter, end of term? */
	  sp[0] = '\0';
	  *chbuffp = '.';
	  *chp = ch;
	  if (has_overflow)
	    return read_int_overflow(s,base,val,sign);
	  if (sign == -1)
	    return MkIntegerTerm(-val);
	  return MkIntegerTerm(val);
	}
      }
#if O_LOCALE
      if ((decimalpoint = (unsigned char*) ( localeconv()->decimal_point )) == NULL)
#endif
	decimalpoint = (const unsigned char*)".";
      dp =(unsigned char *)decimalpoint;
      /* translate . to current locale */
      while ((dc = *dp++) != '\0') {
	*sp++ = dc;
	if (--max_size == 0) {
	  return num_send_error_message("Number Too Long");
	}
      }
      /* numbers after . */
      if (chtype(ch) == NU) {
	do {
	  if (--max_size == 0) {
	    return num_send_error_message("Number Too Long");
	  }
	  *sp++ = ch;
	}
	while (chtype(ch = getchr(inp_stream)) == NU);
      }
    }
    if (ch == 'e' || ch == 'E') {
      if (--max_size == 0) {
	return num_send_error_message("Number Too Long");
      }
      *sp++ = ch;
      ch = getchr(inp_stream);
      if (ch == '-') {
	if (--max_size == 0) {
	  return num_send_error_message("Number Too Long");
	}
	*sp++ = '-';
	ch = getchr(inp_stream);
      } else if (ch == '+') {
	ch = getchr(inp_stream);
      }
      if (chtype(ch) != NU) {
	if (has_dot)
	  return float_send(s,sign);
	return MkIntegerTerm(sign*val);
      }
      do {
	if (--max_size == 0) {
	  return num_send_error_message("Number Too Long");
	}
	*sp++ = ch;
      } while (chtype(ch = getchr(inp_stream)) == NU);
    }
    *sp = '\0';
    *chp = ch;
    return float_send(s,sign);
  } else if (has_overflow) {
    *sp = '\0';
    /* skip base */
    *chp = ch;
    if (s[0] == '0' && s[1] == 'x')
      return read_int_overflow(s+2,16,val,sign);
    else if (s[0] == '0' && s[1] == 'o')
      return read_int_overflow(s+2,8,val,sign);
    else if (s[0] == '0' && s[1] == 'b')
      return read_int_overflow(s+2,2,val,sign);
    if (s[1] == '\'')
      return read_int_overflow(s+2,base,val,sign);
    if (s[2] == '\'')
      return read_int_overflow(s+3,base,val,sign);
    return read_int_overflow(s,base,val,sign);
  } else {
    *chp = ch;
    return MkIntegerTerm(val*sign);
  }
}

/* given a function getchr scan until we  either find the number
   or end of file */
Term
Yap_scan_num(IOSTREAM *inp)
{
  CACHE_REGS
  Term out;
  int sign = 1;
  int ch, cherr;
  char *ptr;

  LOCAL_ErrorMessage = NULL;
  LOCAL_ScannerStack = (char *)TR;
  LOCAL_ScannerExtraBlocks = NULL;
  if (!(ptr = AllocScannerMemory(4096))) {
    LOCAL_ErrorMessage = "Trail Overflow";
    LOCAL_Error_TYPE = OUT_OF_TRAIL_ERROR;	            
    return TermNil;
  }
  ch = getchr(inp);
  while (chtype(ch) == BS) {
    ch = getchr(inp);
  }
  if (ch == '-') {
    sign = -1;
    ch = getchr(inp);
  } else if (ch == '+') {
    ch = getchr(inp);
  }
  if (chtype(ch) != NU) {
    Yap_clean_tokenizer(NULL, NULL, NULL, 0L);
    return TermNil;
  }
  cherr = '\0';
  if (ASP-HR < 1024)
    return TermNil;
  out = get_num(&ch, &cherr, inp, ptr, 4096, sign); /*  */
  PopScannerMemory(ptr, 4096);
  Yap_clean_tokenizer(NULL, NULL, NULL, 0L);
  if (LOCAL_ErrorMessage != NULL || ch != -1 || cherr)
    return TermNil;
  return out;
}


#define CHECK_SPACE() \
	  if (ASP-HR < 1024) { \
	    LOCAL_ErrorMessage = "Stack Overflow";     \
	    LOCAL_Error_TYPE = OUT_OF_STACK_ERROR;	\
	    LOCAL_Error_Size = 0L;	               \
	    if (p) \
	      p->Tok = Ord(kind = eot_tok);           \
	    /* serious error now */                    \
	    return l;                                  \
	  } 


static void
open_comment(int ch, IOSTREAM *inp_stream USES_REGS) {
  CELL *h0 = HR;
  HR += 5;
  h0[0] = AbsAppl(h0+2);
  h0[1] = TermNil;
  if (!LOCAL_CommentsTail) {
    /* first comment */
    LOCAL_Comments = AbsPair(h0);
  } else {
    /* extra comment */
    *LOCAL_CommentsTail = AbsPair(h0);
  }  
  LOCAL_CommentsTail = h0+1;
  h0 += 2;
  h0[0] = (CELL)FunctorMinus;
  h0[1] = Yap_StreamPosition(inp_stream);
  h0[2] = TermNil;
  LOCAL_CommentsNextChar = h0+2;
  LOCAL_CommentsBuff = (wchar_t *)malloc(1024*sizeof(wchar_t));
  LOCAL_CommentsBuffLim = 1024;
  LOCAL_CommentsBuff[0] = ch;
  LOCAL_CommentsBuffPos = 1;
}

static void
extend_comment(int ch USES_REGS) {
  LOCAL_CommentsBuff[LOCAL_CommentsBuffPos] = ch;
  LOCAL_CommentsBuffPos++;
  if (LOCAL_CommentsBuffPos == LOCAL_CommentsBuffLim-1) {
    LOCAL_CommentsBuff = (wchar_t *)realloc(LOCAL_CommentsBuff,sizeof(wchar_t)*(LOCAL_CommentsBuffLim+4096));
    LOCAL_CommentsBuffLim += 4096;
  }
}

static void
close_comment( USES_REGS1 ) {
  LOCAL_CommentsBuff[LOCAL_CommentsBuffPos] = '\0';
  *LOCAL_CommentsNextChar = Yap_WCharsToString(LOCAL_CommentsBuff PASS_REGS);
  free(LOCAL_CommentsBuff);
  LOCAL_CommentsBuff = NULL;
  LOCAL_CommentsBuffLim = 0;
}

static wchar_t *
ch_to_wide(char *base, char *charp)
{
  CACHE_REGS
  int n = charp-base, i;
  wchar_t *nb = (wchar_t *)base;

  if ((nb+n) + 1024 > (wchar_t *)AuxSp) {
    LOCAL_Error_TYPE = OUT_OF_AUXSPACE_ERROR;	  
    LOCAL_ErrorMessage = "Heap Overflow While Scanning: please increase code space (-h)";
    return NULL;
  }
  for (i=n; i > 0; i--) {
    nb[i-1] = (unsigned char)base[i-1];
  }
  return nb+n;
}

#define  add_ch_to_buff(ch) \
  if (wcharp) { *wcharp++ = (ch); if (wcharp >= (wchar_t *)AuxSp-1024) goto huge_var_error; charp = (char *)wcharp; } \
  else { \
    if (ch > MAX_ISO_LATIN1 && !wcharp) { \
      /* does not fit in ISO-LATIN */		\
      wcharp = ch_to_wide(TokImage, charp);	\
      if (!wcharp) goto huge_var_error;		\
      *wcharp++ = (ch); charp = (char *)wcharp; \
    } else { if (charp >= (char *)AuxSp-1024) goto huge_var_error; *charp++ = ch; }	\
  }

#define  add_ch_to_utf8_buff(ch) \
  { if ( (ch &0xff) == ch) { *charp++ = ch; } else \
  { charp = _PL__utf8_put_char(charp, ch); } }

TokEntry *
Yap_tokenizer(IOSTREAM *inp_stream, int store_comments, Term *tposp,  void *rd0)
{
  GET_LD
  TokEntry *t, *l, *p;
  enum TokenKinds kind;
  int solo_flag = TRUE;
  int ch;
  wchar_t *wcharp;
  struct qq_struct_t	       *cur_qq = NULL;
  struct read_data_t *rd = rd0;

  LOCAL_ErrorMessage = NULL;
  LOCAL_Error_Size = 0;
  LOCAL_VarTable = NULL;
  LOCAL_AnonVarTable = NULL;
  LOCAL_ScannerStack = (char *)TR;
  LOCAL_ScannerExtraBlocks = NULL;
  l = NULL;
  p = NULL;			/* Just to make lint happy */
  ch = getchr(inp_stream);
  while (chtype(ch) == BS) {
    ch = getchr(inp_stream);
  }
  *tposp = Yap_StreamPosition(inp_stream);
  Yap_setCurrentSourceLocation( rd );
  LOCAL_StartLine = inp_stream->posbuf.lineno;
  do {
    wchar_t och;
    int quote, isvar;
    char *charp, *mp;
    unsigned int len;
    char *TokImage = NULL;


    t = (TokEntry *) AllocScannerMemory(sizeof(TokEntry));
    t->TokNext = NULL;
    if (t == NULL) {
      LOCAL_ErrorMessage = "Trail Overflow";
      LOCAL_Error_TYPE = OUT_OF_TRAIL_ERROR;	            
      if (p)
	p->Tok = Ord(kind = eot_tok);
      /* serious error now */
      return l;
    }
    if (!l)
      l = t;
    else
      p->TokNext = t;
    p = t;
  restart:
    while (chtype(ch) == BS) {
      ch = getchr(inp_stream);
    }
    t->TokPos = GetCurInpPos(inp_stream);

    switch (chtype(ch)) {

    case CC:
      if (store_comments) {
	CHECK_SPACE();
	open_comment(ch, inp_stream PASS_REGS);
      continue_comment:
	while ((ch = getchr(inp_stream)) != 10 && chtype(ch) != EF) {
	  CHECK_SPACE();
	  extend_comment(ch PASS_REGS);
	}
	CHECK_SPACE();
	extend_comment(ch PASS_REGS);
	if (chtype(ch) != EF) {
	  ch = getchr(inp_stream);
	  if (chtype(ch) == CC) {
	    extend_comment(ch PASS_REGS);
	    goto continue_comment;
	  }
	}
	close_comment( PASS_REGS1 );
      } else {
	while ((ch = getchr(inp_stream)) != 10 && chtype(ch) != EF);
      }
      if (chtype(ch) != EF) {
	/* blank space */
	if (t == l) {
	  /* we found a comment before reading characters */
	  while (chtype(ch) == BS) {
	    ch = getchr(inp_stream);
	  }
	  CHECK_SPACE();
	  *tposp = Yap_StreamPosition(inp_stream);
	  Yap_setCurrentSourceLocation( rd );
	}
	goto restart;
      } else {
	t->Tok = Ord(kind = eot_tok);
      }
      break;

    case UC:
    case UL:
    case LC:
      och = ch;
      ch = getchr(inp_stream);
    scan_name:
      TokImage = ((AtomEntry *) ( Yap_PreAllocCodeSpace()))->StrOfAE;
      charp = TokImage;
      wcharp = NULL;
      isvar = (chtype(och) != LC);
      add_ch_to_buff(och);
      for (; chtype(ch) <= NU; ch = getchr(inp_stream)) {
	if (charp == (char *)AuxSp-1024) {
	huge_var_error:
	  /* huge atom or variable, we are in trouble */
	  LOCAL_ErrorMessage = "Code Space Overflow due to huge atom";
	  LOCAL_Error_TYPE = OUT_OF_AUXSPACE_ERROR;	  
	  Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	  if (p)
	    p->Tok = Ord(kind = eot_tok);
	  /* serious error now */
	  return l;
	}
	add_ch_to_buff(ch);
      }
      while (ch == '\'' && isvar && yap_flags[VARS_CAN_HAVE_QUOTE_FLAG]) {
	if (charp == (char *)AuxSp-1024) {
	  goto huge_var_error;
	}
	add_ch_to_buff(ch);
	ch = getchr(inp_stream);
      }
      add_ch_to_buff('\0');
      if (!isvar) {
	Atom ae;
	/* don't do this in iso */
	if (wcharp) {
	  ae = Yap_LookupWideAtom((wchar_t *)TokImage);
	} else {
	  ae = Yap_LookupAtom(TokImage);
	}
	if (ae == NIL) {
	  LOCAL_Error_TYPE = OUT_OF_HEAP_ERROR;	  
	  LOCAL_ErrorMessage = "Code Space Overflow";
	  if (p)
	    t->Tok = Ord(kind = eot_tok);
	  /* serious error now */
	  return l;
	}
	t->TokInfo = Unsigned(ae);
	Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	if (ch == '(')
	  solo_flag = FALSE;
	t->Tok = Ord(kind = Name_tok);
      } else {
	VarEntry *ve = Yap_LookupVar(TokImage);
	t->TokInfo = Unsigned(ve);
	if (cur_qq) {
	  ve->refs++;
	}
	Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	t->Tok = Ord(kind = Var_tok);
      }
      break;

    case NU:
      {
	int cherr;
	int cha = ch;
	char *ptr;

	cherr = 0;
	if (!(ptr = AllocScannerMemory(4096))) {
	  LOCAL_ErrorMessage = "Trail Overflow";
	  LOCAL_Error_TYPE = OUT_OF_TRAIL_ERROR;	            
	  if (p)
	    t->Tok = Ord(kind = eot_tok);
	  /* serious error now */
	  return l;
	}
	CHECK_SPACE();
	if ((t->TokInfo = get_num(&cha,&cherr,inp_stream,ptr,4096,1)) == 0L) {
	  if (p)
	    p->Tok = Ord(kind = eot_tok);
	  /* serious error now */
	  return l;
	}
	PopScannerMemory(ptr, 4096);
	ch = cha;
	if (cherr) {
	  TokEntry *e;
	  t->Tok = Number_tok;
	  t->TokPos = GetCurInpPos(inp_stream);
	  e = (TokEntry *) AllocScannerMemory(sizeof(TokEntry));
	  if (e == NULL) {
	    LOCAL_ErrorMessage = "Trail Overflow";
	    LOCAL_Error_TYPE = OUT_OF_TRAIL_ERROR;	            
	    if (p)
	      p->Tok = Ord(kind = eot_tok);
	    /* serious error now */
	    return l;
	  } else {
	    e->TokNext = NULL;
	  }
	  t->TokNext = e;
	  t = e;
	  p = e;
	  switch (cherr) {
	  case 'e':
	  case 'E':
	    och = cherr;
	    goto scan_name;
	    break;
	  case '=':
	  case '_':
	    /* handle error while parsing a float */
	    {
	      TokEntry *e2;

	      t->Tok = Ord(Var_tok);
	      t->TokInfo = Unsigned(Yap_LookupVar("E"));
	      t->TokPos = GetCurInpPos(inp_stream);
	      e2 = (TokEntry *) AllocScannerMemory(sizeof(TokEntry));
	      if (e2 == NULL) {
		LOCAL_ErrorMessage = "Trail Overflow";
		LOCAL_Error_TYPE = OUT_OF_TRAIL_ERROR;	            
		if (p)
		  p->Tok = Ord(kind = eot_tok);
		/* serious error now */
		return l;
	      } else {
		e2->TokNext = NULL;
	      }
	      t->TokNext = e2;
	      t = e2;
	      p = e2;
	      if (cherr == '=')
		och = '+';
	      else
		och = '-';
	    }
	    goto enter_symbol;
	  case '+':
	  case '-':
	    /* handle error while parsing a float */
	    {
	      TokEntry *e2;

	      t->Tok = Name_tok;
	      if (ch == '(')
		solo_flag = FALSE;
	      t->TokInfo = Unsigned(AtomE);
	      t->TokPos = GetCurInpPos(inp_stream);
	      e2 = (TokEntry *) AllocScannerMemory(sizeof(TokEntry));
	      if (e2 == NULL) {
		LOCAL_ErrorMessage = "Trail Overflow";
		LOCAL_Error_TYPE = OUT_OF_TRAIL_ERROR;	            
		t->Tok = Ord(kind = eot_tok);
		/* serious error now */
		return l;
	      } else {
		e2->TokNext = NULL;
	      }
	      t->TokNext = e2;
	      t = e2;
	      p = e2;
	    }
	  default:
	    och = cherr;
	    goto enter_symbol;
	  }
	} else {
	  t->Tok = Ord(kind = Number_tok);
	}
      }
      break;

    case QT:
    case DC:
      TokImage = ((AtomEntry *) ( Yap_PreAllocCodeSpace()))->StrOfAE;
      charp = TokImage;
      quote = ch;
      len = 0;
      ch = getchrq(inp_stream);
      wcharp = NULL;

      while (TRUE) {
	if (charp + 1024 > (char *)AuxSp) {
	  LOCAL_Error_TYPE = OUT_OF_AUXSPACE_ERROR;	  
	  LOCAL_ErrorMessage = "Heap Overflow While Scanning: please increase code space (-h)";
	  break;
	}
	if (ch == 10  &&  truePrologFlag(PLFLAG_ISO)) {
	  /* in ISO a new line terminates a string */
	  LOCAL_ErrorMessage = "layout character \n inside quotes";
	  break;
	}
	if (ch == quote) {
	  ch = getchrq(inp_stream);
	  if (ch != quote)
	    break;
	  add_ch_to_buff(ch);
	  ch = getchrq(inp_stream);
	} else if (ch == '\\' && Yap_GetModuleEntry(CurrentModule)->flags & M_CHARESCAPE) {
	  int scan_next = TRUE;
	  if ((ch = read_quoted_char(&scan_next, inp_stream))) {
	    add_ch_to_buff(ch);
	  }
	  if (scan_next) {
	    ch = getchrq(inp_stream);
	  }
	} else if (chtype(ch) == EF && ch <= MAX_ISO_LATIN1) {
	  Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	  t->Tok = Ord(kind = eot_tok);
	  break;
	} else {
	  add_ch_to_buff(ch);
	  ch = getchrq(inp_stream);
	}
	++len;
	if (charp > (char *)AuxSp - 1024) {
	  /* Not enough space to read in the string. */
	  LOCAL_Error_TYPE = OUT_OF_AUXSPACE_ERROR;	  
	  LOCAL_ErrorMessage = "not enough space to read in string or quoted atom";
	  /* serious error now */
	  Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	  t->Tok = Ord(kind = eot_tok);
	  return l;
	}
      }
      if (wcharp) {
	*wcharp = '\0';
      }  else  {
	*charp = '\0';
      }
      if (quote == '"') {
	if (wcharp) {
	  mp = AllocScannerMemory(sizeof(wchar_t)*(len+1));
	} else {
	  mp = AllocScannerMemory(len + 1);
	}
	if (mp == NULL) {
	  LOCAL_ErrorMessage = "not enough heap space to read in string or quoted atom";
	  Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	  t->Tok = Ord(kind = eot_tok);
	  return l;
	}
	if (wcharp) 
	  wcscpy((wchar_t *)mp,(wchar_t *)TokImage);
	else
	  strcpy(mp, TokImage);
	t->TokInfo = Unsigned(mp);
	Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	if (wcharp) {
	  t->Tok = Ord(kind = WString_tok);
	} else {
	  t->Tok = Ord(kind = String_tok);
	}
      } else {
	if (wcharp) {
	  t->TokInfo = Unsigned(Yap_LookupWideAtom((wchar_t *)TokImage));
	} else {
	  t->TokInfo = Unsigned(Yap_LookupAtom(TokImage));
	}
	if (!(t->TokInfo)) {
	  LOCAL_Error_TYPE = OUT_OF_HEAP_ERROR;	  
	  LOCAL_ErrorMessage = "Code Space Overflow";
	  if (p)
	    t->Tok = Ord(kind = eot_tok);
	  /* serious error now */
	  return l;
	}
	Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	t->Tok = Ord(kind = Name_tok);
	if (ch == '(')
	  solo_flag = FALSE;
      }
      break;

    case SY:
      och = ch;
      ch = getchr(inp_stream);
      if (och == '/' && ch == '*') {
	if (store_comments) {
	  CHECK_SPACE();
	  open_comment('/', inp_stream PASS_REGS);
	  while ((och != '*' || ch != '/') && chtype(ch) != EF) {
	    och = ch;
	    CHECK_SPACE();
	    extend_comment(ch PASS_REGS);
	    ch = getchr(inp_stream);
	  }
	  if (chtype(ch) != EF) {
	    CHECK_SPACE();
	    extend_comment(ch PASS_REGS);
	  }
	  close_comment( PASS_REGS1 );
	} else {
	  while ((och != '*' || ch != '/') && chtype(ch) != EF) {
	    och = ch;
	    ch = getchr(inp_stream);
	  }
	}
	if (chtype(ch) == EF) {
	  t->Tok = Ord(kind = eot_tok);
	} else {
	  /* leave comments */
	  ch = getchr(inp_stream);
	  if (t == l) {
	    /* we found a comment before reading characters */
	    while (chtype(ch) == BS) {
	      ch = getchr(inp_stream);
	    }
	    CHECK_SPACE();
	    *tposp = Yap_StreamPosition(inp_stream);
	    Yap_setCurrentSourceLocation( rd );
	  }
	}
	goto restart;
      }
    enter_symbol:
      if (och == '.' && (chtype(ch) == BS || chtype(ch) == EF
			 || chtype(ch) == CC)) {
	if (chtype(ch) == CC)
	  while ((ch = getchr(inp_stream)) != 10 && chtype(ch) != EF);
	t->Tok = Ord(kind = eot_tok);
      } else {
	Atom ae;
	TokImage = ((AtomEntry *) ( Yap_PreAllocCodeSpace()))->StrOfAE;
	charp = TokImage;
	wcharp = NULL;
	add_ch_to_buff(och);
	for (; chtype(ch) == SY; ch = getchr(inp_stream)) {
	  if (charp == (char *)AuxSp-1024) {
	    goto huge_var_error;
	  }
	  add_ch_to_buff(ch);
	}
	add_ch_to_buff('\0');
	if (wcharp) {
	  ae = Yap_LookupWideAtom((wchar_t *)TokImage);
	} else {
	  ae = Yap_LookupAtom(TokImage);
	}
	if (ae == NIL) {
	  LOCAL_Error_TYPE = OUT_OF_HEAP_ERROR;	  
	  LOCAL_ErrorMessage = "Code Space Overflow";
	  if (p)
	    t->Tok = Ord(kind = eot_tok);
	  /* serious error now */
	  return l;
	}
	t->TokInfo = Unsigned(ae);
	if (t->TokInfo == (CELL)NIL) {
	  LOCAL_Error_TYPE = OUT_OF_HEAP_ERROR;	  
	  LOCAL_ErrorMessage = "Code Space Overflow";
	  if (p)
	    t->Tok = Ord(kind = eot_tok);
	  /* serious error now */
	  return l;
	}
	Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	t->Tok = Ord(kind = Name_tok);
	if (ch == '(')
	  solo_flag = FALSE;
	else
	  solo_flag = TRUE;
      }
      break;
    
    case SL:
      {
	char chs[2];
	chs[0] = ch;
	chs[1] = '\0';
	ch = getchr(inp_stream);
	t->TokInfo = Unsigned(Yap_LookupAtom(chs));
	t->Tok = Ord(kind = Name_tok);
	if (ch == '(')
	  solo_flag = FALSE;
      }
      break;

    case BK:
      och = ch;
      ch = getchr(inp_stream);
      t->TokInfo = och;
      if (och == '(')  {
	while (chtype(ch) == BS) {  ch = getchr(inp_stream); };
	if (ch == ')') {
	  t->TokInfo = Unsigned(AtomEmptyBrackets);
	  t->Tok = Ord(kind = Name_tok);
	  ch = getchr(inp_stream);
	  solo_flag = FALSE;
	  break;
	} else if (!solo_flag) {
	  t->TokInfo = 'l';
	  solo_flag = TRUE;
	}
      } else if (och == '[')  {
	while (chtype(ch) == BS) {  ch = getchr(inp_stream); };
	if (ch == ']') {
	  t->TokInfo = Unsigned(AtomNil);
	  t->Tok = Ord(kind = Name_tok);
	  ch = getchr(inp_stream);
	  solo_flag = FALSE;
	  break;
	}
      } else if (och == '{')  {
       if (ch == '|') {
	 qq_t *qq = (qq_t *)calloc(sizeof(qq_t), 1);
	 if (!qq) {
	  LOCAL_ErrorMessage = "not enough heap space to read in quasi quote";
	  Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	  t->Tok = Ord(kind = eot_tok);
	  return l;
	 }
	 if (cur_qq) {
	  LOCAL_ErrorMessage = "quasi quote in quasi quote";
	  Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	  t->Tok = Ord(kind = eot_tok);
	  free( qq );
	  return l;
	 } else {
	   cur_qq = qq;
	 }
	 t->TokInfo = (CELL)qq;
	 qq->start.byteno = inp_stream->position->byteno;
	 qq->start.lineno = inp_stream->position->lineno;
	 qq->start.linepos = inp_stream->position->linepos - 1;
	 qq->start.charno = inp_stream->position->charno - 1;
         t->Tok = Ord(kind = QuasiQuotes_tok);
         ch = getchr(inp_stream);
         solo_flag = FALSE;
         break;
       }
       while (chtype(ch) == BS) {  ch = getchr(inp_stream); };
       if (ch == '}') {
         t->TokInfo = Unsigned(AtomBraces);
         t->Tok = Ord(kind = Name_tok);
         ch = getchr(inp_stream);
         solo_flag = FALSE;
         break;
       }
      } else if (och == '|' && ch == '|') {
	qq_t *qq = cur_qq;
	if (!qq) {
	  LOCAL_ErrorMessage = "quasi quoted's || without {|";
	  Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	  free( cur_qq );
	  cur_qq = NULL;
	  t->Tok = Ord(kind = eot_tok);
	  return l;
	}
	cur_qq = NULL;
	t->TokInfo = (CELL)qq;
	qq->mid.byteno = inp_stream->position->byteno;
	qq->mid.lineno = inp_stream->position->lineno;
	qq->mid.linepos = inp_stream->position->linepos - 1;
	qq->mid.charno = inp_stream->position->charno - 1;
	t->Tok = Ord(kind = QuasiQuotes_tok);
	ch = getchr(inp_stream);

	TokImage = Yap_PreAllocCodeSpace();
	if (!TokImage) {
	  LOCAL_ErrorMessage = "not enough heap space to read in a quasi quoted atom";
	  Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	  t->Tok = Ord(kind = eot_tok);
	  return l;
	}
	charp = TokImage;
	quote = ch;
	len = 0;
	ch = getchrq(inp_stream);
	wcharp = NULL;

	while (TRUE) {
	  if (ch == '|') {
	    ch = getchrq(inp_stream);
	    if (ch != '}') {
	    } else {
	      add_ch_to_utf8_buff(och);
	      add_ch_to_utf8_buff(ch);
	      /* we're done */
	      break;
	    }
	  } else if (chtype(ch) == EF) {
	    Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	    t->Tok = Ord(kind = eot_tok);
	    break;
	  } else {
	    add_ch_to_utf8_buff(ch);
	    ch = getchrq(inp_stream);
	  }
	  if (charp > (char *)AuxSp - 1024) {
	    /* Not enough space to read in the string. */
	    LOCAL_Error_TYPE = OUT_OF_AUXSPACE_ERROR;	  
	    LOCAL_ErrorMessage = "not enough space to read in string or quoted atom";
	    /* serious error now */
	    Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	    t->Tok = Ord(kind = eot_tok);
	    return l;
	  }
	}
	len = charp-TokImage;
	mp = malloc(len + 1);
	if (mp == NULL) {
	  LOCAL_ErrorMessage = "not enough heap space to read in quasi quote";
	  Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	  t->Tok = Ord(kind = eot_tok);
	  return l;
	}
	strncpy(mp, TokImage, len+1);
	qq->text = (unsigned char *)mp;
	Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	qq->end.byteno = inp_stream->position->byteno;
	qq->end.lineno = inp_stream->position->lineno;
	qq->end.linepos = inp_stream->position->linepos - 1;
	qq->end.charno = inp_stream->position->charno - 1;
	if (!(t->TokInfo)) {
	  LOCAL_Error_TYPE = OUT_OF_HEAP_ERROR;	  
	  LOCAL_ErrorMessage = "Code Space Overflow";
	  if (p)
	    t->Tok = Ord(kind = eot_tok);
	  /* serious error now */
	  return l;
	}
	Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	solo_flag = FALSE;
	ch = getchr(inp_stream);
	break;
      }
      t->Tok = Ord(kind = Ponctuation_tok);
      break;
    case EF:
      t->Tok = Ord(kind = eot_tok);
      break;

    default:
#if DEBUG
      fprintf(stderr, "\n++++ token: wrong char type %c %d\n", ch, chtype(ch));
#endif
      t->Tok = Ord(kind = eot_tok);
    }
#if DEBUG
    if(GLOBAL_Option[2]) fprintf(stderr,"[Token %d %ld]",Ord(kind),(unsigned long int)t->TokInfo);
#endif
    if (LOCAL_ErrorMessage) {
      /* insert an error token to inform the system of what happened */
      TokEntry *e = (TokEntry *) AllocScannerMemory(sizeof(TokEntry));
      if (e == NULL) {
	LOCAL_ErrorMessage = "Trail Overflow";
	LOCAL_Error_TYPE = OUT_OF_TRAIL_ERROR;	            
	p->Tok = Ord(kind = eot_tok);
	/* serious error now */
	return l;
      }
      p->TokNext = e;
      e->Tok = Error_tok;
      e->TokInfo = MkAtomTerm(Yap_LookupAtom(LOCAL_ErrorMessage));
      e->TokPos = GetCurInpPos(inp_stream);
      e->TokNext = NULL;
      LOCAL_ErrorMessage = NULL;
      p = e;
    }
  } while (kind != eot_tok);
  return (l);
}

void
Yap_clean_tokenizer(TokEntry *tokstart, VarEntry *vartable, VarEntry *anonvartable, Term commentable)
{
  CACHE_REGS
  struct scanner_extra_alloc *ptr = LOCAL_ScannerExtraBlocks;
  while (ptr) {
    struct scanner_extra_alloc *next = ptr->next;
    free(ptr);
    ptr = next;
  }
  LOCAL_Comments = TermNil;
  LOCAL_CommentsNextChar = LOCAL_CommentsTail = NULL;
  if (LOCAL_CommentsBuff) {
    free(LOCAL_CommentsBuff);
    LOCAL_CommentsBuff = NULL;
  }
  LOCAL_CommentsBuffLim = 0;
}


/// @}
