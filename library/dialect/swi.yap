
% SWI emulation.
% written in an on-demand basis.

%%
%% @file dialect/swi.yap
%%
%% @defgroup SWI Compatibility with SWI-Prolog and Other Prolog systems
%% @{
%% @ingroup YAPProgramming

/**

@defgroup System SWI Dialect Support
@ingroup SWI

@{

This library provides a number of SWI-Prolog builtins that are not by
default in YAP. This support is loaded with the
~~~~~
expects_dialect(swi)
~~~~~

 command.

*/

/** @pred  time_file(+ _File_,- _Time_)


Unify the last modification time of  _File_ with
 _Time_.  _Time_ is a floating point number expressing the seconds
elapsed since Jan 1, 1970.


*/
/** @pred concat_atom(+ _List_,- _Atom_)



 _List_ is a list of atoms, integers or floating point numbers. Succeeds
if  _Atom_ can be unified with the concatenated elements of  _List_. If
 _List_ has exactly 2 elements it is equivalent to `atom_concat/3`,
allowing for variables in the list.


*/

:- module(system, [concat_atom/2,
		   concat_atom/3,
		   read_clause/1,
		   chdir/1,
		   compile_aux_clauses/1,
		   convert_time/2,
		   convert_time/8,
		   '$declare_module'/5,
		   '$set_predicate_attribute'/3,
		   stamp_date_time/3,
		   date_time_stamp/2,
		   time_file/2,
		   flag/3,
		   require/1,
		   normalize_space/2,
		   current_flag/1
		]).

:- reexport(library(charsio),[
			      write_to_chars/2,
			      read_from_chars/2
			     ]).

:- reexport(library(lists),[append/2,
			    append/3,
			    delete/3,
			    member/2,
			    flatten/2,
			    intersection/3,
			    last/2,
			    memberchk/2,
			    max_list/2,
			    min_list/2,
			    nextto/3,
			    permutation/2,
			    reverse/2,
			    select/3,
			    selectchk/3,
			    sublist/2,
			    sumlist/2,
			    nth1/4,
			    nth0/4,
			    nth1/3,
			    nth0/3]).

:- reexport(library(apply),[maplist/2,
			    maplist/3,
			    maplist/4,
			    maplist/5,
			    include/3,
			    exclude/3,
			    partition/4,
			    partition/5
			   ]).

:- reexport(library(system),
	      [datime/1,
	       mktime/2,
	       file_property/2,
	       delete_file/1]).

:- reexport(library(arg),
	      [genarg/3]).

:- reexport(library(apply_macros),
	      []).

:- reexport(library(terms),
	      [subsumes/2,
	       subsumes_chk/2,
	       term_hash/2,
	       unifiable/3,
	       cyclic_term/1,
	       variant/2]).

:- source.

:- style_check(all).

:- yap_flag(unknown,error).

:- yap_flag(open_expands_filename,false).

:- yap_flag(autoload,true).

:- set_prolog_flag(user_flags,silent).


% Time is given as a float in SWI-Prolog.
swi_get_time(FSecs) :- datime(Datime),  mktime(Datime, Secs), FSecs is Secs*1.0.

goal_expansion(atom_concat(A,B),atomic_concat(A,B)).
/** @pred  atom_concat(? _A1_,? _A2_,? _A12_) is iso

The predicate holds when the third argument unifies with an atom, and
the first and second unify with atoms such that their representations
concatenated are the representation for  _A12_.

If  _A1_ and  _A2_ are unbound, the built-in will find all the atoms
that concatenated give  _A12_.


*/

goal_expansion(atom_concat(A,B,C),atomic_concat(A,B,C)).
%goal_expansion(arg(A,_,_),_) :- nonvar(A), !, fail.
goal_expansion(arg(A,B,C),arg:genarg(A,B,C)).

% make sure we also use
:- user:library_directory(X),
	atom(X),
	atom_concat([X,'/dialect/swi'],SwiDir),
	\+ user:library_directory(SwiDir),
	asserta(user:library_directory(SwiDir)),
	fail
	;
	true.

:- multifile
   user:file_search_path/2.

:- dynamic
   user:file_search_path/2.

/** @pred concat_atom(? _List_,+ _Separator_,? _Atom_)


Creates an atom just like concat_atom/2, but inserts  _Separator_
between each pair of atoms.  For example:

~~~~~
?- concat_atom([gnu, gnat], ', ', A).

A = 'gnu, gnat'
~~~~~

(Unimplemented) This predicate can also be used to split atoms by
instantiating  _Separator_ and  _Atom_:

~~~~~
?- concat_atom(L, -, 'gnu-gnat').

L = [gnu, gnat]
~~~~~


*/
concat_atom([A|List], Separator, New) :- var(List), !,
	atom_codes(Separator,[C]),
	atom_codes(New, NewChars),
	split_atom_by_chars(NewChars,C,L,L,A,List).
concat_atom(List, Separator, New) :-
	add_separator_to_list(List, Separator, NewList),
	atomic_concat(NewList, New).


split_atom_by_chars([],_,[],L,A,[]):-
	atom_codes(A,L).
split_atom_by_chars([C|NewChars],C,[],L,A,[NA|Atoms]) :- !,
	atom_codes(A,L),
	split_atom_by_chars(NewChars,C,NL,NL,NA,Atoms).
split_atom_by_chars([C1|NewChars],C,[C1|LF],LAtom,Atom,Atoms) :-
	split_atom_by_chars(NewChars,C,LF,LAtom,Atom,Atoms).

add_separator_to_list([], _, []).
add_separator_to_list([T], _, [T]) :- !.
add_separator_to_list([H|T], Separator, [H,Separator|NT]) :-
	add_separator_to_list(T, Separator, NT).

concat_atom(List, New) :-
	atomic_concat(List, New).


bindings_message(V) -->
       { cvt_bindings(V, Bindings) },
       prolog:message(query(_YesNo,Bindings)), !.

cvt_bindings([],[]).
cvt_bindings([[Name|Value]|L],[AName=Value|Bindings]) :-
	atom_codes(AName, Name),
	cvt_bindings(L,Bindings).

/** @pred chdir(+ _Dir_)

Compatibility predicate.  New code should use working_directory/2.
*/
chdir(X) :- cd(X).

%%	convert_time(+Stamp, -String)
%
%	Convert  a time-stamp as  obtained though get_time/1 into a  textual
%	representation  using the C-library function ctime().  The  value is
%	returned  as a  SWI-Prolog string object  (see section  4.23).   See
%	also convert_time/8.
%
%	@deprecated Use format_time/3.


convert_time(Stamp, String) :-
	format_time(string(String), '%+', Stamp).

%%	convert_time(+Stamp, -Y, -Mon, -Day, -Hour, -Min, -Sec, -MilliSec)
%
%	Convert   a  time  stamp,   provided  by   get_time/1,   time_file/2,
%	etc.   Year is  unified with the year,  Month with the month  number
%	(January  is 1), Day  with the day of  the month (starting with  1),
%	Hour  with  the hour  of the  day (0--23),  Minute  with the  minute
%	(0--59).   Second with the  second (0--59) and MilliSecond with  the
%	milliseconds  (0--999).  Note that the latter might not  be accurate
%	or  might always be 0, depending  on the timing capabilities of  the
%	system.  See also convert_time/2.
%
%	@deprecated Use stamp_date_time/3.

convert_time(Stamp, Y, Mon, Day, Hour, Min, Sec, MilliSec) :-
	stamp_date_time(Stamp,
			date(Y, Mon, Day,
			     Hour, Min, FSec,
			     _, _, _),
			local),
	Sec is integer(float_integer_part(FSec)),
	MilliSec is integer(float_fractional_part(FSec)*1000).


compile_aux_clauses([]).
compile_aux_clauses([(:- G)|Cls]) :- !,
	prolog_load_context(module, M),
	once(M:G),
	compile_aux_clauses(Cls).
compile_aux_clauses([Cl|Cls]) :-
	prolog_load_context(module, M),
	assert_static(M:Cl),
	compile_aux_clauses(Cls).


flag(Key, Old, New) :-
	recorded(Key, Old, R), !,
	(
	 Old \== New
	->
	 erase(R),
	 recorda(Key, New, _)
	;
	 true
	).
flag(Key, 0, New) :-
	functor(Key, N, Ar),
	functor(K, N, Ar),
	assert(flag(K)),
	recorda(K, New, _).

current_flag(Key) :-
	flag(Key).

require(F) :-
	must_be_list(F),
	% notice that this must be used as a declaration.
	prolog_load_context(module, Mod),
	required_predicates(F, Mod).

required_predicates([], _).
required_predicates([F|Fs], M) :-
	required_predicate(F, M),
	required_predicates(Fs, M).

required_predicate(Na/Ar, M) :-
	functor(G, Na, Ar),
	(
	 predicate_property(M:G, _) ->
	 true
	;
         autoloader:find_predicate(G, _)
	).

/**
@}

@}
*/
