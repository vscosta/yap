:- dynamic exported/2, defines_module/1.

:- include(yapops).

%:- use_module(library(lists)).
append([], L, L).
append([H|T], L, [H|R]) :-
       append(T, L, R).
append(ListOfLists, List) :-
%	must_be_list( ListOfLists),
	append_(ListOfLists, List).

append_([], []).
append_([L], L).
append_([L1,L2|Ls], L) :-
	append(L1,L2,LI),
	append_([LI|Ls],L).


:- use_module(library(maplist)).

%:- use_module(library(system)).
%:- use_module(library(matrix)).
%:- use_module(library(readutil)).
%:- use_module(library(lineutils)).

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

```
?- split("Hello * I am free"," *",S).

S = ["Hello","I","am","free"] ?

no
```

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
	split_at_blank(SplitCodes,More).
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

encode_text(A,NS) :-
    atom(A),
    !,
    atom_string(A,S),
    encode_text(S,NS).
encode_text(S,NS) :-
    sub_string(S,Bef,1,End," "),
    !,
    sub_string(S,0,Bef,_,S1),
    encode(S1,NS1),
    sub_string(S,_,End,0,S2),
    encode_text(S2,NS2),
    string_concat([NS1,' ',NS2], NS).
encode_text(S,NS) :-
    encode(S,NS).



encode("","") :-
    !.
encode(Pred/A,String) :-
    !,
    (atom(Pred) ->
     atom_string(Pred, SPred)
     ;
     Pred = SPred
    ),
    pred2dox(SPred, String0),
    (number(A) ->
     number_string(A, Arity)
     ;
    atom(A) ->
    atom_string(A, Arity);
    A = Arity
    ),
    string_concat([String0,"_",Arity],String).
encode(Pred, Pred).

  decode(A,SF):-
    atom(A),
    !,
    atom_string(A,S),
    decode(S,SF).
  decode(S,SF):-
    sub_string(S,_,1,1,"_"),
    sub_string(S,_,1,0,C),
    string_codes(C,[CN]),
    code_type_digit(CN),
   sub_string(S,0,_,2,Name),
		 !,
     dox2pred(Name,Name1),
     string_concat([Name1,"/",C],SF).
  decode(S,SF):-
    dox2pred(S,SF),
    !.
decode(P,P).


pred2dox(Pred, String) :-
    string_codes(Pred,Chars),
    fetch_chars(Chars,Codes,[]),
    !,
    string_codes(String,Codes).
pred2dox(Pred,Pred).

    dox2pred(String,Pred) :-
    string_codes(String,Codes),
    fetch_chars(Chars,Codes,[]),
    !,
    string_codes(Pred,Chars).
    dox2pred(String,String).

fetch_chars([]) -->[], !.
fetch_chars([C|Cs]) -->
{var(C)},
    [0'_],
    [A,B],
    {A >= "A", A< "A"+16,
     B >= "A", B< "A"+16
     },
     !,
     {
C is (A-"A")*16+(B-"A")
    },
fetch_chars(Cs).
fetch_chars([C|Cs]) -->
    {nonvar(C),
    \+ code_type_alnum(C)},
!,    {
A is (C div 16)+"A",
B is (C mod 16)+"A"
},
    [0'_,A,B],
fetch_chars(Cs).
fetch_chars([C|Cs]) -->
    [C],
fetch_chars(Cs).

