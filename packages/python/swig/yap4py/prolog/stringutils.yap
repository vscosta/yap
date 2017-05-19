/**
 * @file   stringutils.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Wed Nov 18 01:14:42 2015
 * 
 * @brief Simple string utilitiities .
 * 
 * 
*/
:- module(string_utils,
	  [string/1,
	   upcase_string/2,
	   downcase_string/2,
	   string_length/2,
	   concat_strings/3
	  ]).

:- use_module(library(lists),
	      [append/3]).

string([]).
string([A|Cs]) :-
	integer(A),
	A > 0, % no EOF allowed
	A < 0x3ffffff, % UNICODE characters, strictly not yet required.
	string(Cs).

upcase_string([], []).
upcase_string([C|Cs], [NC|NCs]) :-
	code_type(C,to_lower(NC)),
	upcase_string(Cs, NCs).

downcase_string([], []).
downcase_string([C|Cs], [NC|NCs]) :-
	code_type(C,to_upper(NC)),
	downcase_string(Cs, NCs).

string_length(S, Length) :-
	length(S, Length).

concat_strings(S1, S2, New) :-
	append(S1, S2, New).

