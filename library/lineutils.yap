
:- module(line_utils,
	  [search_for/2,
	   search_for/3,
	   scan_natural/3,
	   scan_integer/3,
	   split/2,
	   split/3,
	   fields/2,
	   fields/3,
	   glue/3,
	   copy_line/2,
	   filter/3,
	   file_filter/3,
	   file_filter_with_initialization/5,
	   process/2
	  ]).

/** @defgroup LineUtilities Line Manipulation Utilities
@ingroup YAPLibrary
@{

This package provides a set of useful predicates to manipulate
sequences of characters codes, usually first read in as a line. It is
available by loading the 
~~~~
:- use_module(library(lineutils)).
~~~~

*/

:- meta_predicate
	filter(+,+,2),
	file_filter(+,+,2),
	file_filter_with_initialization(+,+,2,+,:),
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

~~~~~{.prolog}
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
	split(SplitCodes, New, More).
split_at_blank(_, []) --> [].

split(SplitCodes, [], More) -->
	[C],
	{ member(C, SplitCodes) }, !,
	split_at_blank(SplitCodes, More).
split(SplitCodes, [C|New], Set) -->
	[C], !,
	split(SplitCodes, New, Set).
split(_, [], []) --> [].

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

~~~~~{.prolog}
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
~~~~~{.prolog}
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
	 format(StreamOut, '~s~n', [NewLine]),
	 fail
	).

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


/** @pred file_filter(+ _FileIn_, + _FileOut_, + _Goal_)  is meta

For every line  _LineIn_ in file  _FileIn_, execute
`call(Goal,LineIn,LineOut)`, and output  _LineOut_ to file
 _FileOut_.
*/
file_filter(Inp, Out, Command) :-
	open(Inp, read, StreamInp),
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
	open(Inp, read, StreamInp),
	open(Out, write, StreamOut),
	format(StreamOut, FormatString, Parameters),
	filter(StreamInp, StreamOut, Command),
	close(StreamInp),
	close(StreamOut).
/**
@}
*/
