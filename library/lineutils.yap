/**
 * @file   lineutils.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 22:02:22 2015
 * 
 * @brief  line text processing.
 * 
 * 
*/

:- module(lineutils,
	  [search_for/2,
	   search_for/3,
	   scan_natural/3,
	   scan_integer/3,
	   natural/3,
	   integer/3,
	   blank/3,
	   split/2,
	   split/3,
	   split/4,
	   split/5,
       split_unquoted/3,
	   fields/2,
	   fields/3,
	   glue/3,
	   copy_line/2,
	   filter/1,
	   filter/3,
	   file_filter/3,
       file_select/2,
		file_filter_with_initialization/5,
		file_filter_with_start_end/5,
		file_filter_with_initialization/5 as file_filter_with_init,
	   process/2
	  ]).

/** @defgroup line_utils Line Manipulation Utilities
@ingroup library
@{

This package provides a set of useful predicates to manipulate
sequences of characters codes, usually first read in as a line. It is
available by loading the
~~~~
:- use_module(library(lineutils)).
~~~~


*/

:- meta_predicate
	filter(v -2),
	filter(+,+,2),
	file_filter(+,+,2),
	file_filter_with_initialization(+,+,2,+,:),
	file_filter_with_start_end(+,+,2,2,2),
	process(+,1).

:- use_module(library(lists),
	      [member/2,
	       append/3]).

:- use_module(library(readutil),
	      [read_line_to_codes/2]).

/**
 @pred search_for(+ _Char_,+ _Line_)
  Search for a character  _Char_ in the list of codes  _Line_.
*/
search_for(C,L) :-
	search_for(C, L, []).

search_for(C) --> [C], !.
search_for(C) --> [_],
	search_for(C).

/** @pred scan_integer(? _Int_,+ _Line_,+ _RestOfLine_)

Scan the list of codes  _Line_ for an integer  _Nat_, either a
positive, zero, or negative integer, and unify  _RestOfLine_ with
the remainder of the line.
*/
scan_integer(N) -->
	"-", !,
	scan_natural(0, N0),
	N is -N0.
scan_integer(N) -->
	scan_natural(0, N).

/** @pred integer(? _Int_,+ _Line_,+ _RestOfLine_)

Scan the list of codes  _Line_ for an integer  _Nat_, either a
positive, zero, or negative integer, and unify  _RestOfLine_ with
the remainder of the line.
*/
integer(N) -->
	"-", !,
	natural(0, N0),
	N is -N0.
vzinteger(N) -->
	natural(0, N).

/** @pred scan_natural(? _Nat_,+ _Line_,+ _RestOfLine_)

Scan the list of codes  _Line_ for a natural number  _Nat_, zero
or a positive integer, and unify  _RestOfLine_ with the remainder
of the line.
*/
scan_natural(N) -->
	scan_natural(0, N).

scan_natural(N0,N) -->
	[C],
	{C >= 0'0, C =< 0'9 }, !,
	{ N1 is N0*10+(C-0'0) }, %'
	get_natural(N1,N).
scan_natural(N,N) --> [].

/** @pred natural(? _Nat_,+ _Line_,+ _RestOfLine_)

Scan the list of codes  _Line_ for a natural number  _Nat_, zero
or a positive integer, and unify  _RestOfLine_ with the remainder
of the line.
*/
natural(N) -->
	natural(0, N).

natural(N0,N) -->
	[C],
	{C >= 0'0, C =< 0'9 }, !,
	{ N1 is N0*10+(C-0'0) }, %'
	get_natural(N1,N).
natural(N,N) --> [].

/** @pred skip_whitespace(+ _Line_,+ _RestOfLine_)

Scan the list of codes  _Line_ for white space,  namely for tabbing and space characters.
*/
skip_whitespace([0' |Blanks]) -->
	" ",
	skip_whitespace( Blanks ).
skip_whitespace([0'	|Blanks]) -->
	"	",
	skip_whitespace( Blanks ).
skip_whitespace( [] ) -->
	!.

/** @pred blank(+ _Line_,+ _RestOfLine_)

 The list of codes  _Line_ is formed by white space,  namely by tabbing and space characters.
*/
blank([0' |Blanks]) -->
	" ",
	blank( Blanks ).
blank([0'	|Blanks]) -->
	"	",
	blank( Blanks ).
blank( [] ) -->
	[].


/** @pred split(+ _Line_,- _Split_)

Unify  _Words_ with a set of strings obtained from  _Line_ by
using the blank characters  as separators.
*/
split(String, Strings) :-
	split_at_blank(" 	", Strings, String, []).

/** @pred split(+ _Line_,+ _Separators_,- _Split_)



Unify  _Words_ with a set of strings obtained from  _Line_ by
using the character codes in  _Separators_ as separators. As an
example, consider:

~~~~~
?- split("Hello * I am free"," *",S).

S = ["Hello","I","am","free"] ?

no
~~~~~

*/
split(String, SplitCodes, Strings) :-
	split_at_blank(SplitCodes, Strings, String, []).

split_at_blank(SplitCodes, More) -->
	[C],
	{ member(C, SplitCodes) }, !,
	split_at_blank(SplitCodes, More).
split_at_blank(SplitCodes, [[C|New]| More]) -->
	[C], !,
	split_(SplitCodes, New, More).
split_at_blank(_, []) --> [].

split_(SplitCodes, [], More) -->
	[C],
	{ member(C, SplitCodes) }, !,
	split_at_blank(SplitCodes, More).
split_(SplitCodes, [C|New], Set) -->
	[C], !,
	split_(SplitCodes, New, Set).
split_(_, [], []) --> [].


split(Text, SplitCodes, DoubleQs, SingleQs, Strings) :-
	split_element(SplitCodes, DoubleQs, SingleQs, Strings, Text, []).

split_element(SplitCodes,  DoubleQs, SingleQs, Strings) -->
    [C],
    !,
    split_element(SplitCodes,  DoubleQs, SingleQs, Strings, C).
split_element(_SplitCodes,  _DoubleQs, _SingleQs, []) --> !.
split_element(_SplitCodes,  _DoubleQs, _SingleQs, [[]]) --> [].

split_element(SplitCodes,  DoubleQs, SingleQs, Strings, C) -->
	{ member( C, SingleQs ) },
	!,
	 [C2],
	{ Strings = [[C2|String]|More] },
	split_element(SplitCodes,  DoubleQs, SingleQs, [String| More]).
split_element(SplitCodes,  DoubleQs, SingleQs, [[]|Strings], C) -->
	{ member( C, SplitCodes ) },
	!,
	split_element(SplitCodes,  DoubleQs, SingleQs, Strings).
split_element(SplitCodes,  DoubleQs, SingleQs, Strings, C) -->
	{ member( C, DoubleQs ) } ,
	!,
	split_within(SplitCodes,  C-DoubleQs, SingleQs, Strings).
split_element(SplitCodes,  DoubleQs, SingleQs, [[C|String]|Strings], C) -->
	split_element(SplitCodes,  DoubleQs, SingleQs, [String|Strings]). 

split_within(SplitCodes,  DoubleQs, SingleQs, Strings) -->
    [C],
    split_within(SplitCodes,  DoubleQs, SingleQs, Strings, C).

split_within(SplitCodes,  DoubleQs, SingleQs, Strings, C) -->
	{ member( C, SingleQs ) },
	!,
	 [C2],
	{ Strings = [[C2|String]|More] },
	split_within(SplitCodes,  DoubleQs, SingleQs, [String| More]).
split_within(SplitCodes,  DoubleQs, C-SingleQs, Strings, C) -->
	!,
	split_element(SplitCodes,  DoubleQs, SingleQs, Strings).
split_within(SplitCodes,  DoubleQs, SingleQs, [[C|String]|Strings], C) -->
	split_within(SplitCodes,  DoubleQs, SingleQs, [String|Strings]). 

/** @pred split_unquoted(+ _Line_,+ _Separators_,- _Split_)



Unify  _Words_ with a set of strings obtained from  _Line_ by
using the character codes in  _Separators_ as separators, but treat text wi
thin double quotes as a single unit. As an
example, consider:

~~~~~
?- split("Hello * I \"am free\""," *",S).

S = ["Hello","I","am free"] ?

no
~~~~~

*/
split_unquoted(String, SplitCodes, Strings) :-
        split_unquoted_at_blank(SplitCodes, Strings, String, []).

split_unquoted_at_blank(SplitCodes, [[0'"|New]|More]) --> %0'"
    "\"",
    split_quoted(New, More),
    split_unquoted_at_blank(SplitCodes, More).
split_unquoted_at_blank(SplitCodes, More) -->
        [C],
        { member(C, SplitCodes) }, !,
        split_unquoted_at_blank(SplitCodes, More).
split_unquoted_at_blank(SplitCodes, [[C|New]| More]) -->
        [C], !,
        split_unquoted(SplitCodes, New, More).
split_unquoted_at_blank(_, []) --> [].

split_unquoted(SplitCodes, [], More) -->
        [C],
        { member(C, SplitCodes) }, !,
        split_unquoted_at_blank(SplitCodes, More).
split_unquoted(SplitCodes, [C|New], Set) -->
        [C], !,
        split_unquoted(SplitCodes, New, Set).
split_unquoted(_, [], []) --> [].


/** @pred split_quoted(+ _Line_,+ _Separators_, GroupQuotes, SingleQuotes, - _Split_)



Unify  _Words_ with a set of strings obtained from  _Line_ by
using the character codes in  _Separators_ as separators, but treat text within  quotes as a single unit. As an
example, consider:

~~~~~
?- split_quoted("Hello * I \"am free\""," *",S).

S = ["Hello","I","am free"] ?

no
~~~~~

*/
split_quoted( [0'"], _More) --> %0'"
    "\"".
split_quoted( [0'\\ ,C|New], More) --> 
    %0'"
    "\\",
    [C],
    split_quoted(New, More).
split_quoted( [C|New], More) --> %0'"
    [C],
    split_quoted(New, More).

/** @pred fields(+ _Line_,- _Split_)

Unify  _Words_ with a set of strings obtained from  _Line_ by
using the blank characters  as field separators.

*/
fields(String, Strings) :-
	fields(" 	", Strings, String, []).

/** @pred fields(+ _Line_,+ _Separators_,- _Split_)

Unify  _Words_ with a set of strings obtained from  _Line_ by
using the character codes in  _Separators_ as separators for
fields. If two separators occur in a row, the field is considered
empty. As an example, consider:

~~~~~
?- fields("Hello  I am  free"," *",S).

  S = ["Hello","","I","am","","free"] ?
~~~~~
*/
fields(String, FieldsCodes, Strings) :-
	dofields(FieldsCodes, First, More, String, []),
	(
	  First = [], More = []
	->
	  Strings = []
	;
	  Strings = [First|More]
	).

dofields(FieldsCodes, [], New.More) -->
	[C],
	{ member(C, FieldsCodes) }, !,
	dofields(FieldsCodes, New, More).
dofields(FieldsCodes, [C|New], Set) -->
	[C], !,
	dofields(FieldsCodes, New, Set).
dofields(_, [], []) --> [].

/** @pred glue(+ _Words_,+ _Separator_,- _Line_)

Unify  _Line_ with  string obtained by glueing  _Words_ with
the character code  _Separator_.
*/
glue([], _, []).
glue([A], _, A) :- !.
glue([H|T], [B|_], Merged) :-
	append(H, [B|Rest], Merged),
	glue(T, [B], Rest).

/** @pred copy_line(+ _StreamInput_,+ _StreamOutput_)

Copy a line from  _StreamInput_ to  _StreamOutput_.
*/
copy_line(StreamInp, StreamOut) :-
	read_line_to_codes(StreamInp, Line),
	format(StreamOut, '~s~n', [Line]).


/** @pred filter(+ _StreamInp_, + _StreamOut_, + _Goal_)

For every line  _LineIn_ in stream  _StreamInp_, execute
`call(Goal,LineIn,LineOut)`, and output  _LineOut_ to
stream  _StreamOut_. If `call(Goal,LineIn,LineOut)` fails,
nothing will be output but execution continues with the next
line. As an example, consider a procedure to select the second and
fifth field of a CSV table :
~~~~~
select(Sep, In, Out) :-
	fields(In, Sep, [_,F2,_,_,F5|_]),
        fields(Out,Sep, [F2,F5]).

select :-
       filter(",",
~~~~~

*/
filter(StreamInp, StreamOut, Command) :-
	repeat,
	read_line_to_codes(StreamInp, Line),
	(
	 Line == end_of_file
	->
	 !
	;
	 call(Command, Line, NewLine),
	 ground(NewLine),
	 format(StreamOut, '~s~n', [NewLine]),
	 fail
	).

filter(G) :-
	filter(user_input, user_output, G).

/** @pred process(+ _StreamInp_, + _Goal_) is meta

For every line  _LineIn_ in stream  _StreamInp_, call
`call(Goal,LineIn)`.
*/
process(StreamInp, Command) :-
	repeat,
	read_line_to_codes(StreamInp, Line),
	(
	 Line == end_of_file
	->
	 !
	;
	 call(Command, Line),
	 fail
	).

 
/**
  * @pred file_filter(+ _FileIn_, + _FileOut_, + _Goal_)  is meta
  *
  * @param _FileIn_  File to process
  * @param _FileOut_ Output file, often user_error
  * @param _Goal_ to be metacalled, receives FileIn and FileOut as
  * extra arguments
  *
  * @return succeeds

  For every line  _LineIn_ in file  _FileIn_, execute
  `call(Goal,LineIn,LineOut)`, and output  _LineOut_ to file
  _FileOut_.

  The input stream is accessible through the alias `filter_input`, and
  the output stream is accessible through `filter_output`.
*/
file_filter(Inp, Out, Command) :-
	open(Inp, read, StreamInp, [alias(filter_input)]),
	open(Out, write, StreamOut),
	filter(StreamInp, StreamOut, Command),
	close(StreamInp),
	close(StreamOut).

/** @pred file_filter_with_initialization(+ _FileIn_, + _FileOut_, + _Goal_, + _FormatCommand_,   + _Arguments_)

Same as file_filter/3, but before starting the filter execute
`format/3` on the output stream, using  _FormatCommand_ and
 _Arguments_.
*/
file_filter_with_initialization(Inp, Out, Command, FormatString, Parameters) :-
	open(Inp, read, StreamInp, [alias(filter_input)]),
	open(Out, write, StreamOut, [alias(filter_output)]),
	format(StreamOut, FormatString, Parameters),
	filter(StreamInp, StreamOut, Command),
	close(StreamInp),
	close(StreamOut).


/** @pred file_filter_with_start_end(+ FileIn, + FileOut, + Goal, + StartGoal,   + EndGoal)

Same as file_filter/3, but before starting the filter execute
_StartGoal_,  and call _ENdGoal_ as an epilog.

The input stream are always accessible through `filter_output` and `filter_input`.
*/
file_filter_with_start_end(Inp, Out, Command, StartGoal, EndGoal) :-
	open(Inp, read, StreamInp, [alias(filter_input)]),
	open(Out, write, StreamOut, [alias(filter_output)]),
	call( StartGoal, StreamInp, StreamOut ),
	filter(StreamInp, StreamOut, Command),
	call( EndGoal, StreamInp, StreamOut ),
	close(StreamInp),
	close(StreamOut).


/**
  * @pred file_select(+ _FileIn_, + _Goal_)  is meta
  *
  * @param _FileIn_  File or Stream to process
  * @param _Goal_ to be metacalled, receives FileIn as
  * extra arguments
  *
  * @return  bindings to arguments of _Goal_.
  *
  * @brief For every line  _LineIn_ in file  _FileIn_, execute
  * `call(`Goal,LineIn)`.
  *
  * The input stream is accessible through the alias `filter_input`, and
  * the output stream is accessible through `filter_output`.
*/
file_select(Inp, Command) :-
	( retract(alias(F)) -> true ; F = '' ),
	atom_concat(filter_input, F, Alias),
        open(Inp, read, StreamInp, [Alias]),
	atom_concat('_', F, NF),
	assert( alias(NF) ),
	repeat,
	read_line_to_codes(StreamInp, Line),
	(
	 Line == end_of_file
	->
	 close(StreamInp),
	 retract(alias(NF)),
	 assert(alias(F)),
	  !,
          atom_concat(filter_input, F, Alias),
	  fail
	;
	 call(Command, Line)
        ).

/**
@}
*/
