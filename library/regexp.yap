/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************

/**
 * @file   regexp.yap
 *
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 *  from BSD Unix work.
 * @date   Wed Nov 18 00:27:52 2015
 *
 * @brief  Support for Regular Expressions	in YAP
 *
 *
*/


:- module(regexp, [
	regexp/3,
	regexp/4
          ]).


/** @defgroup regexp Regular Expressions
@ingroup library
@{

This library includes routines to determine whether a regular expression
matches part or all of a string. The routines can also return which
parts parts of the string matched the expression or subexpressions of
it. This library relies on Henry Spencer's `C`-package and is only
available in operating systems that support dynamic loading. The
`C`-code has been obtained from the sources of FreeBSD-4.0 and is
protected by copyright from Henry Spencer and from the Regents of the
University of California (see the file library/regex/COPYRIGHT for
further details).

Much of the description of regular expressions below is copied verbatim
from Henry Spencer's manual page.

A regular expression is zero or more branches, separated by ``|`.  It
matches anything that matches one of the branches.

A branch is zero or more pieces, concatenated.  It matches a match for
the first, followed by a match for the second, etc.

A piece is an atom possibly followed by `\*`, `+`, or `?`.  An atom
followed by `\*` matches a sequence of 0 or more matches of the atom.
An atom followed by `+` matches a sequence of 1 or more matches of the
atom.  An atom followed by `?` matches a match of the atom, or the
null string.

An atom is a regular expression in parentheses (matching a match for the
regular expression), a range (see below), `.`  (matching any single
character), `^` (matching the null string at the beginning of the
input string), `$` (matching the null string at the end of the input
string), a `\` followed by a single character (matching that
character), or a single character with no other significance (matching
that character).

A range is a sequence of characters enclosed in `[]`.  It normally
matches any single character from the sequence.  If the sequence begins
with `^`, it matches any single character not from the rest of the
sequence.  If two characters in the sequence are separated by `-`,
this is shorthand for the full list of ASCII characters between them
(e.g. `[0-9]` matches any decimal digit).  To include a literal `]`
in the sequence, make it the first character (following a possible
`^`).  To include a literal `-`, make it the first or last
character.

*/

  /**
 @pred regexp(+ _RegExp_,+ _String_,+ _Opts_)

Match regular expression  _RegExp_ to input string  _String_
according to options  _Opts_. The options may be:

+ `nocase`: Causes upper-case characters  in   _String_ to
be treated  as  lower case during the matching process.



*/

/** @pred regexp(+ _RegExp_,+ _String_,+ _Opts_,? _SubMatchVars_)


Match regular expression  _RegExp_ to input string  _String_
according to options  _Opts_. The variable  _SubMatchVars_ should
be originally unbound or a list of unbound variables all will contain a
sequence of matches, that is, the head of  _SubMatchVars_ will
contain the characters in  _String_ that matched the leftmost
parenthesized subexpression within  _RegExp_, the next head of list
will contain the characters that matched the next parenthesized
subexpression to the right in  _RegExp_, and so on.

The options may be:

+ `nocase`: Causes upper-case characters  in   _String_ to
be treated  as  lower case during the matching process.
+ `indices`: Changes what  is  stored  in
 _SubMatchVars_. Instead  of storing the matching characters from
 _String_, each variable will contain a term of the form  _IO-IF_
giving the indices in  _String_ of the first and last characters  in
the  matching range of characters.



In general there may be more than one way to match a regular expression
to an input string.  For example,  consider the command

~~~~~
  regexp("(a*)b*","aabaaabb", [], [X,Y])
~~~~~
Considering only the rules given so far,  _X_ and  _Y_ could end up
with the values `"aabb"` and `"aa"`, `"aaab"` and
`"aaa"`, `"ab"` and `"a"`, or any of several other
combinations.  To resolve this potential ambiguity `regexp` chooses among
alternatives using the rule `first then longest`.  In other words, it
considers the possible matches in order working from left to right
across the input string and the pattern, and it attempts to match longer
pieces of the input string before shorter ones.  More specifically, the
following rules apply in decreasing order of priority:

+ If a regular expression could match  two  different parts of an
input string then it will match the one that begins earliest.

+ If a regular expression contains "|"  operators  then the leftmost matching sub-expression is chosen.

+ In \*, +, and ? constructs, longer matches are chosen in preference to shorter ones.

+ In sequences of expression  components  the  components are considered from left to right.

In the example above, `"(a\*)b\*"` matches `"aab"`: the
`"(a\*)"` portion of the pattern is matched first and it consumes
the leading `"aa"`; then the `"b\*"` portion of the pattern
consumes the next `"b"`.  Or, consider the following example:

~~~~~
  regexp("(ab|a)(b*)c",  "abc", [], [X,Y,Z])
~~~~~

After this command  _X_ will be `"abc"`,  _Y_ will be
`"ab"`, and  _Z_ will be an empty string.  Rule 4 specifies that
`"(ab|a)"` gets first shot at the input string and Rule 2 specifies
that the `"ab"` sub-expression is checked before the `"a"`
sub-expression.  Thus the `"b"` has already been claimed before the
`"(b\*)"` component is checked and `(b\*)` must match an empty string.




 */
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


/** @} */
